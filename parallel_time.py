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

def runMP(numbers):
    start = time.time()

    p = Pool()
    result = p.map(sum_square, numbers)
    print(result)

    p.close()
    p.join()

    finish = time.time()

    print(f'Parallel: {round(finish-start, 4)} seconds')

def runSR(numbers):
    start = time.time()

    result = []

    for i in numbers:
        result.append(sum_square(i))

    finish = time.time()

    print(f'Serial: {round(finish-start, 4)} seconds')

if __name__ == '__main__':

    numbers = range(1000, 10000, 1) # start large to work each process

    runMP(numbers)
    runSR(numbers)
