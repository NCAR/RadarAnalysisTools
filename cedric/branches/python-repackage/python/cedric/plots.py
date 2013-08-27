"Plotting utilities related to CEDRIC and radar data."

import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec

def inspect_field(field):
    print(field[::25,::25,0])
    CS = plt.contourf(field[::4,::4,0], levels=xrange(-5,55,10))
    cbar = plt.colorbar(CS)
    plt.show()
    plt.close()


def contourf(field, levels):
    # The height of the slice comes from the third index, assuming this
    # was sliced horizontally...
    iheight = field.index()[2]
    grid = field.volume().grid
    height = grid.z.first + iheight*grid.z.delta
    cs = plt.contourf(field.data(), levels)
    cbar = plt.colorbar(cs)
    plt.title("%s / %s @ %f %s" % (field.volume().title, field.name(),
                                   height, "km"))

def compare_fields(f1, f2, levels=xrange(-60,60,5)):
    "Create a figure with side-by-side plots of the 2-D variables."
    fig = plt.figure(figsize=(10, 5))
    gs = gridspec.GridSpec(1, 2)
    axleft = plt.subplot(gs[0])
    contourf(f1, levels)
    axright = plt.subplot(gs[1])
    contourf(f2, levels)
    return fig
