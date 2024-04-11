# Trying out parallel in python
# based on: https://www.youtube.com/watch?v=fKl2JW_qrso

import time
import concurrent.futures
import multiprocessing

start = time.perf_counter()

def do_something(secs):
    print(f'Sleeping {secs} seconds...')
    time.sleep(secs)
    return 'Done sleeping'

with concurrent.futures.ProcessPoolExecutor() as executor:
    f1 = executor.submit(do_something, 1)
    print(f1.result())

# Multiprocessing toolbox - didn't work.
# processes = []

# for _ in range(10):
#     p = multiprocessing.Process(target = do_something)
#     p.start()
#     processes.append(p)

# for process in processes:
#     process.join()

finish = time.perf_counter()

print(f'Finished in {round(finish-start, 2)} seconds')
