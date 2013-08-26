"Volume and Variable classes for radar data, and other file-related methods."

import os
import struct
import numpy as np
import copy
import logging

logger = logging.getLogger(__name__)

from collections import namedtuple
from collections import OrderedDict


Axis = namedtuple('Axis', 'first delta n')
Grid = namedtuple('Grid', 'refx refy x y z')


class Volume(OrderedDict):
    """
    An ordered container for radar fields along with their volume metadata.

    The metadata includes dates and times and important grid
    specifications, as well as the basic project and scientist fields.  All
    of the volume information, including the gridded data themselves, can
    be read into a Volume instance using a method like
    cedric.read_volume().
    """

    def __init__(self):
        
        OrderedDict.__init__(self)
        self.title = None
        self.bdate = None
        self.btime = None
        self.edate = None
        self.etime = None
        self.project = None
        self.scientist = None
        self.station = None
        self.output_cs = None
        self.grid = None
        
        self.vars = self
        self.data = {}
        self.nfld = 0

    def __str__(self):
        vfields = [ "%s=%s" % (str(key), str(val)) 
                    for key, val in self.__dict__.items() ]
        vfields.sort()
        return("volume:\n%s" % ("\n".join(vfields)))


class Variable(object):
    """
    The Variable class contains the radar field metadata and the data array.

    A Variable instance can be indexed like a numpy array, and the indexing
    subset will be remembered and carried forward with the metadata, so
    that other methods--such as those in cedric.plots--can retrieve the
    field name and the level for annotations.
    """

    def __init__(self, vid, name, scale, volume):
        self._vid = vid
        self._name = name
        self._scale = scale
        self._volume = volume
        self._data = None
        self._index = None

    def __getitem__(self, index):
        "Return a copy of this Variable with a different index into the data."
        self.data()
        nv = copy.copy(self)
        nv._data = self._data.__getitem__(index)
        nv._index = index
        return nv

    def name(self):
        return self._name

    def volume(self):
        return self._volume

    def scale(self):
        return self._scale

    def index(self):
        return self._index

    def data(self):
        if self._data is None:
            self._data = self._volume.data[self._name]
        return self._data
        
    def nid(self):
        return self._vid


def _coordinates_from_words(vwords, index):
    (x0, xm, nx, dx) = vwords[index:index+4]
    x0 = x0 / 100.0
    xm = xm / 100.0
    dx = dx / 1000.0
    return (x0, xm, nx, dx)


def volumeFromWords(v, vwords):
    """
    Construct a Volume instance from the CEDRIC 510-word volume header.

    Typically the volume header array is retrieved from the CEDRIC library
    common block after a volume has been read from a file.
    """
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

    (refx, refy) = [ x / 100.0 for x in vwords[40:42] ]
    (x0, xm, nx, dx) = _coordinates_from_words(vwords, 159)
    (y0, ym, ny, dy) = _coordinates_from_words(vwords, 164)
    (z0, zm, nz, dz) = _coordinates_from_words(vwords, 169)

    v.grid = Grid(refx=refx, refy=refy,
                  x = Axis(x0, dx, nx),
                  y = Axis(y0, dy, ny),
                  z = Axis(z0, dz, nz))

    v.nfld = vwords[174]
    logger.debug("nfld=%s" % (v.nfld))
    for ii in range(v.nfld):
        index = 175+ii*5
        varn = str(struct.pack('<4h', *vwords[index:index+4])).rstrip()
        scale = vwords[179+ii*5]
        vfield = Variable(ii+1, varn, float(scale), v)
        v[varn] = vfield
        logger.debug("variable: %s" % (str(vfield)))
    return v


# Superceded by reading volumes directly through the cedric library, so
# hide it.
class _CedricFile:
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

        for var in v:
            valid  = var.data() != -32768.
            var.data()[valid]  /= var.scale()
            var.data()[valid == 0] = -1000.

        return v

