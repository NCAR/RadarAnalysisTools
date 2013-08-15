"Plotting utilities related to CEDRIC and radar data."

import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec

def inspect_field(field):
    print(field[::25,::25,0])
    CS = plt.contourf(field[::4,::4,0], levels=xrange(-5,55,10))
    cbar = plt.colorbar(CS)
    plt.show()
    plt.close()


def contourf(field):
    # The height of the slice comes from the third index, assuming this
    # was sliced horizontally...
    iheight = field.index()[2]
    grid = field.volume().grid
    height = grid.z.first + iheight*grid.z.delta
    levels=xrange(-5,55,10)
    cs = plt.contourf(field.data(), levels)
    cbar = plt.colorbar(cs)
    plt.title("%s / %s @ %f %s" % (field.volume().title, field.name(),
                                   height, "km"))

def compare_fields(f1, f2):
    "Create a figure with side-by-side plots of the 2-D variables."
    fig = plt.figure(figsize=(20,10))
    gs = gridspec.GridSpec(1, 2)
    axleft = plt.subplot(gs[0])
    contourf(f1)
    axright = plt.subplot(gs[1])
    contourf(f2)
    plt.show()
    plt.close()
