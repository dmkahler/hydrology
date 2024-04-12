# Trying out parallel in python
# based on: https://www.youtube.com/watch?v=fKl2JW_qrso - didn't work
# based on: https://www.youtube.com/watch?v=GT10PnUFLlE - error in multiprocessing - no join, comparison was degenerate


import time
import multiprocessing as mp

results_a = []
results_b = []
results_c = []

def calc_a(numbers):
    for number in numbers:
        results_a.append(number**2)

def calc_b(numbers):
    for number in numbers:
        results_b.append(number**3)

def calc_c(numbers):
    for number in numbers:
        results_c.append(number**4)

start = time.time()
processes = []

if __name__ == '__main__':
    number_list = list(range(100000))

    p1 = mp.Process(target = calc_a, args = (number_list,))
    p2 = mp.Process(target = calc_b, args = (number_list,))
    p3 = mp.Process(target = calc_c, args = (number_list,))

    p1.start()
    p2.start()
    p3.start()

finish = time.time()

print(f'Finished in {round(finish-start, 3)} seconds')

# for number list 1e5:
# in order: 0.071 seconds
# in multiprocess 