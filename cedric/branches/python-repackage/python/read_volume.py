
import logging
logging.basicConfig(level=logging.DEBUG)

import cedric
import matplotlib.pyplot as plt


if __name__ == "__main__":

    filepath = "../testdata/spol/vol_20000629_233038_to_20000629_233433_SPOL_CRT.ced"
    core = cedric.Cedric()
    core.read_volume(filepath)
    field = core.fetchd(15, 2)
    print(field)
    plt.contourf(field)
    plt.show()

    core.quit()




