# Trying out parallel in python
# based on: https://www.youtube.com/watch?v=fKl2JW_qrso - didn't work
# based on: https://www.youtube.com/watch?v=GT10PnUFLlE 

import time
# from multiprocessing import mp

results_a = []
results_b = []
results_c = []

def calc_a(numbers):
    for number in numbers:
        results_a.append(number**2)

def calc_b(numbers):
    for number in numbers:
        results_a.append(number**3)

def calc_c(numbers):
    for number in numbers:
        results_a.append(number**4)

if __name__ == '__main__':
    number_list = list(range(100000))
    start = time.perf_counter()
    calc_a(number_list)
    calc_b(number_list)
    calc_c(number_list)
    finish = time.perf_counter()

print(f'Finished in {round(finish-start, 3)} seconds')
