# Trying out parallel in python
# based on: https://www.youtube.com/watch?v=fKl2JW_qrso

import time

start = time.perf_counter()

def do_something():
    print('Sleeping 1 second...')
    time.sleep(1)
    print('Done sleeping')

do_something()
do_something()

finish = time.perf_counter()

print(f'Finished in {round(finish-start, 2)} seconds')
