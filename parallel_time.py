# Trying out parallel in python
# based on: https://www.youtube.com/watch?v=fKl2JW_qrso - didn't work
# based on: https://www.youtube.com/watch?v=GT10PnUFLlE - error in multiprocessing - no join, comparison was degenerate
# based on: https://www.youtube.com/watch?v=u2jTn-Gj2Xw

import time
from multiprocessing import Pool

def sum_square(number):
    s = 0
    for i in range(number):
        s += i * i
    return(s)

start = time.time()

if __name__ == '__main__':

    numbers = range(10)

    p = Pool()
    result = p.map(sum_square, numbers)
    print(result)

    p.close()
    p.join()

finish = time.time()

print(f'Finished in {round(finish-start, 3)} seconds')

# for number list 1e5:
# in order: 0.071 seconds
# in multiprocess 