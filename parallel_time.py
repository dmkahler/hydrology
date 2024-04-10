# Trying out parallel in python
# based on: https://www.youtube.com/watch?v=fKl2JW_qrso

import time
import multiprocessing

start = time.perf_counter()

def do_something(seconds):
    print(f'Sleeping {seconds} seconds...')
    time.sleep(seconds)
    print('Done sleeping')

processes = []

for _ in range(10):
    p = multiprocessing.Process(target = do_something)
    p.start()
    processes.append(p)

for process in processes:
    process.join()

finish = time.perf_counter()

print(f'Finished in {round(finish-start, 2)} seconds')
