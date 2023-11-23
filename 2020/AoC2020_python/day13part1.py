import numpy as np

x = None

#t = 939
#line_ids = [7,13,x,x,59,x,31,19]

t = 1003681
line_ids = [
    23,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,431,x,
    x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,
    x,x,x,x,409,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,x,
    x,x,x,x,x,x,x,x,29,
]

line_ids = np.array([i for i in line_ids if i is not None])
index = np.argmin(-(t % -line_ids))

print(line_ids[index], -(t % -line_ids[index]))
print(line_ids[index] * -(t % -line_ids[index]))
