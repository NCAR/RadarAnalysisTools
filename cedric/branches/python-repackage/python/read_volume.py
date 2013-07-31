
import sys
import logging
logging.basicConfig(level=logging.DEBUG)

import cedric
import matplotlib.pyplot as plt


if __name__ == "__main__":

    filepath = "../testdata/spol/vol_20000629_233038_to_20000629_233433_SPOL_CRT.ced"
    if len(sys.argv) > 1:
        filepath = sys.argv[1]

    core = cedric.Cedric()
    core.read_volume(filepath)
    core.stats()
    field = core.fetchd(3, 10)
    print(field)
    plt.contourf(field)
    plt.show()

    core.quit()




