
acc = $100

acc_byte_0 = $170
acc_byte_1 = $171
acc_byte_2 = $172
acc_byte_3 = $173
acc_byte_4 = $174
acc_byte_5 = $175
acc_byte_6 = $176

%macro ld64 %location
  ld %location
%end

%macro push64acc
  push acc
  push acc_byte_6
  push acc_byte_5
  push acc_byte_4
  push acc_byte_3
  push acc_byte_2
  push acc_byte_1
  push acc_byte_0
%end

%macro pop64acc
  pop acc_byte_0
  pop acc_byte_1
  pop acc_byte_2
  pop acc_byte_3
  pop acc_byte_4
  pop acc_byte_5
  pop acc_byte_6
  pop acc
%end

%macro push32acc
  push acc
  push acc_byte_2
  push acc_byte_1
  push acc_byte_0
%end

%macro pop32acc
  pop acc_byte_0
  pop acc_byte_1
  pop acc_byte_2
  pop acc
%end

%macro push16acc
  push acc
  push acc_byte_0
%end

%macro pop16acc
  pop acc_byte_0
  pop acc
%end

%macro push8acc 
  push acc
%end

%macro pop8acc
  pop acc
%end

%macro push64 %location
  push %location
  push %location + 1
  push %location + 2
  push %location + 3
  push %location + 4
  push %location + 5
  push %location + 6
  push %location + 7
%end

%macro pop64 %location
  pop %location + 7
  pop %location + 6
  pop %location + 5
  pop %location + 4
  pop %location + 3
  pop %location + 2
  pop %location + 1
  pop %location
%end

%macro push32 %location
  push %location
  push %location + 1
  push %location + 2
  push %location + 3

%end

%macro pop32 %location
  pop %location + 3
  pop %location + 2
  pop %location + 1
  push %location
%end

%macro push16 %location
  push %location
  push %location + 1
%end

%macro pop16 %location
  pop %location + 1
  pop %location
%end

%macro push8 %location
  push %location
%end

%macro pop8 %location
  pop %location
%end
