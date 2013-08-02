"CedricFile class module."

import os
import struct
import numpy as np
import logging

logger = logging.getLogger(__name__)

from collections import namedtuple
from collections import OrderedDict

Axis = namedtuple('Axis', 'first delta n')
Grid = namedtuple('Grid', 'refx refy x y z')
Variable = namedtuple('Variable', 'id name scale')

class Volume:
    pass


def _coordinates_from_words(vwords, index):
    (x0, xm, nx, dx) = vwords[index:index+4]
    x0 = x0 / 100.0
    xm = xm / 100.0
    dx = dx / 1000.0
    return (x0, xm, nx, dx)


def volumeFromWords(v, vwords):
    "Construct a Volume from the CEDRIC 510-word volume header."

    v.bdate = (vwords[115]*100 + vwords[116])*100 + vwords[117]
    v.btime = (vwords[118]*100 + vwords[119])*100 + vwords[120]
    v.edate = (vwords[121]*100 + vwords[122])*100 + vwords[123]
    v.etime = (vwords[124]*100 + vwords[125])*100 + vwords[126]

    # The volume header is accessed as words first, with the endianness
    # of the system that wrote it.  Then those words (after possibly being
    # byte-swapped) are converted to strings.

    hlen = vwords[60]
    logger.debug("volume header length (should be 510): %i" % (hlen))

    # Repack the header words into a string so they can be unpacked as
    # character strings.
    (v.project, v.scientist, v.station, v.output_cs) = struct.unpack(
        '4s6s6s4s', struct.pack('<10h', *vwords[7:17]))
    logger.debug(str(v.__dict__))

    (v.refx, v.refy) = [ x / 100.0 for x in vwords[40:42] ]
    (v.x0, v.xm, v.nx, v.dx) = _coordinates_from_words(vwords, 159)
    (v.y0, v.ym, v.ny, v.dy) = _coordinates_from_words(vwords, 164)
    (v.z0, v.zm, v.nz, v.dz) = _coordinates_from_words(vwords, 169)
    logger.debug(str(v.__dict__))

    v.grid = Grid(refx=v.refx, refy=v.refy,
                  x = Axis(v.x0, v.dx, v.nx),
                  y = Axis(v.y0, v.dy, v.ny),
                  z = Axis(v.z0, v.dz, v.nz))

    v.vars = OrderedDict()
    v.data = {}
    v.nfld = vwords[174]
    logger.debug("nfld=%s" % (v.nfld))
    for ii in range(v.nfld):
        index = 175+ii*5
        varn = str(struct.pack('<4h', *vwords[index:index+4])).rstrip()
        scale = vwords[179+ii*5]
        vfield = Variable(ii+1, varn, float(scale))
        v.vars[varn] = vfield
        logger.debug("variable: %s" % (str(vfield)))
    vfields = ["%s=%s" % (str(key), str(val)) for key, val in v.__dict__.items()]
    vfields.sort()
    logger.debug("volume:\n%s" % ("\n".join(vfields)))
    return v


class CedricFile:
    "Extract file and volume information from the CEDRIC file format."

    def __init__(self, filepath):
        self.readFile(filepath)
        self.filepath = filepath


    def readFile(self, filepath):
        fced = open(filepath, 'rb')

        #read file header 1540 byte
        file_head = fced.read(1540)
        logger.debug("reading CEDRIC file: %s" % (filepath))
        logger.debug("file header: %s" % (file_head[0:4]))
        little_endian, = struct.unpack('i', file_head[4:8])
        self.ec = ['>', '<'][bool(little_endian)]
        filesize, = struct.unpack(self.ec+'i', file_head[8:12])
        logger.debug("endian=%i, filesize=%i" % (little_endian, filesize))

        self.volume_offsets = struct.unpack(self.ec+'25i', file_head[16:116])
        print("volume_offsets = ", self.volume_offsets)
        self.volume_descriptions = []
        for i in xrange(25):
            if not self.volume_offsets[i]:
                continue
            begin = 116 + 56*i
            end = begin + 56
            self.volume_descriptions.append(
                struct.unpack('56s', file_head[begin:end])[0])
        logger.debug("volume_descriptions:\n%s" %
                     ("\n".join(self.volume_descriptions)))
        self.volumes = []
        for i in xrange(25):
            offset = self.volume_offsets[i]
            if offset:
                v = self._read_volume(fced, offset)
                self.volumes.append(v)
                break


    def _read_volume(self, fced, offset):

        fced.seek(offset, os.SEEK_SET)
        logger.debug("reading volume at offset %i" % (fced.tell()))
        #read volume header 510 2-byte
        vol_head = fced.read(510*2)
        v = Volume()
        vwords = struct.unpack(self.ec+'510h', vol_head)

        v = volumeFromWords(v, vwords)

        for ll in range(v.nz):
            #read levDBZ
            lvl_head = fced.read(10*2)
            for ff in range(v.nfld):
                #read field
                buf  = fced.read(2 * v.nx * v.ny)
                tmp  = np.asarray(np.fromstring(buf, self.ec + 'i2'), 
                                  dtype=np.float32)
                tmp.shape = v.nx, v.ny
                v.data[v.name_ls[ff]][:,:,ll] = tmp

        for ii, kk in enumerate(v.data.keys()):
            valid  = v.data[kk]!=-32768.
            invalid= v.data[kk]==-32768.
            v.data[kk][valid]  /= v.vars[v.name_ls[ii]]
            v.data[kk][invalid] = -1000.

        return v

