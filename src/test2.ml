; ModuleID = 'LILY'
source_filename = "LILY"

@"print_return!2" = global i64 0
@"arg_return!2" = global i64 0
@"d!0" = global i64 0
@"print_return!1" = global i8 0
@"arg4!0" = global i8 0
@"arg3!1" = global i8 0
@"arg2!1" = global i8 0
@"arg1!1" = global i8 0
@"arg_return!1" = global i8 0
@"print_return!0" = global i8 0
@"arg3!0" = global i8 0
@"arg2!0" = global i8 0
@"arg1!0" = global i8 0
@"arg_return!0" = global i8 0
@"a!0" = global i8 0
@fmt = private unnamed_addr constant [5 x i8] c"%lu\0A\00", align 1
@fmt.1 = private unnamed_addr constant [16 x i8] c"%c %c %c %c %c\0A\00", align 1
@fmt.2 = private unnamed_addr constant [13 x i8] c"%c %c %c %c\0A\00", align 1

define i8 @main() {
entry:
  %listlitmalloc = tail call ptr @malloc(i32 mul (i32 ptrtoint (ptr getelementptr (i8, ptr null, i32 1) to i32), i32 24))
  %listlitmallocint = ptrtoint ptr %listlitmalloc to i64
  store i64 2, ptr %listlitmalloc, align 4
  %ptradd = add i64 8, ptr %listlitmalloc
  store i64 5, i64 %ptradd, align 4
  %ptradd1 = add i64 16, ptr %listlitmalloc
  store i64 3, i64 %ptradd1, align 4
  store ptr %listlitmalloc, ptr @"d!0", align 8
  %"print!2_result" = call i64 @"print!2"(i64 0)
  ret i8 0
}

define i64 @"print!2"(i64 %arg_return) {
entry:
  %arg_return1 = alloca i64, align 8
  store i64 %arg_return, ptr %arg_return1, align 4
  store i64 %arg_return, ptr @"arg_return!2", align 4
  %"arg_return!2" = load i64, ptr @"arg_return!2", align 4
  %printf = call i64 @printf(ptr @fmt, i64 %"arg_return!2")
  store i64 %printf, ptr @"print_return!2", align 4
  %"arg_return!22" = load i64, ptr @"arg_return!2", align 4
  ret i64 %"arg_return!22"
}

define i8 @"print_bool!0"(i8 %a) {
entry:
  %a1 = alloca i8, align 1
  store i8 %a, ptr %a1, align 1
  store i8 %a, ptr @"a!0", align 1
  %"a!0" = load i8, ptr @"a!0", align 1
  %tmp = icmp eq i8 %"a!0", 1
  br i1 %tmp, label %then, label %else

then:                                             ; preds = %entry
  %"print!0_result" = call i8 @"print!0"(i8 116, i8 114, i8 117, i8 101)
  br label %if_end

else:                                             ; preds = %entry
  %"print!1_result" = call i8 @"print!1"(i8 102, i8 97, i8 108, i8 115, i8 101)
  br label %if_end

if_end:                                           ; preds = %else, %then
  %"a!02" = load i8, ptr @"a!0", align 1
  ret i8 %"a!02"
}

define i8 @"print!1"(i8 %arg_return, i8 %arg1, i8 %arg2, i8 %arg3, i8 %arg4) {
entry:
  %arg_return1 = alloca i8, align 1
  store i8 %arg_return, ptr %arg_return1, align 1
  store i8 %arg_return, ptr @"arg_return!1", align 1
  %arg12 = alloca i8, align 1
  store i8 %arg1, ptr %arg12, align 1
  store i8 %arg1, ptr @"arg1!1", align 1
  %arg23 = alloca i8, align 1
  store i8 %arg2, ptr %arg23, align 1
  store i8 %arg2, ptr @"arg2!1", align 1
  %arg34 = alloca i8, align 1
  store i8 %arg3, ptr %arg34, align 1
  store i8 %arg3, ptr @"arg3!1", align 1
  %arg45 = alloca i8, align 1
  store i8 %arg4, ptr %arg45, align 1
  store i8 %arg4, ptr @"arg4!0", align 1
  %"arg4!0" = load i8, ptr @"arg4!0", align 1
  %"arg3!1" = load i8, ptr @"arg3!1", align 1
  %"arg2!1" = load i8, ptr @"arg2!1", align 1
  %"arg1!1" = load i8, ptr @"arg1!1", align 1
  %"arg_return!1" = load i8, ptr @"arg_return!1", align 1
  %printf = call i8 @printf(ptr @fmt.1, i8 %"arg_return!1", i8 %"arg1!1", i8 %"arg2!1", i8 %"arg3!1", i8 %"arg4!0")
  store i8 %printf, ptr @"print_return!1", align 1
  %"arg_return!16" = load i8, ptr @"arg_return!1", align 1
  ret i8 %"arg_return!16"
}

define i8 @"print!0"(i8 %arg_return, i8 %arg1, i8 %arg2, i8 %arg3) {
entry:
  %arg_return1 = alloca i8, align 1
  store i8 %arg_return, ptr %arg_return1, align 1
  store i8 %arg_return, ptr @"arg_return!0", align 1
  %arg12 = alloca i8, align 1
  store i8 %arg1, ptr %arg12, align 1
  store i8 %arg1, ptr @"arg1!0", align 1
  %arg23 = alloca i8, align 1
  store i8 %arg2, ptr %arg23, align 1
  store i8 %arg2, ptr @"arg2!0", align 1
  %arg34 = alloca i8, align 1
  store i8 %arg3, ptr %arg34, align 1
  store i8 %arg3, ptr @"arg3!0", align 1
  %"arg3!0" = load i8, ptr @"arg3!0", align 1
  %"arg2!0" = load i8, ptr @"arg2!0", align 1
  %"arg1!0" = load i8, ptr @"arg1!0", align 1
  %"arg_return!0" = load i8, ptr @"arg_return!0", align 1
  %printf = call i8 @printf(ptr @fmt.2, i8 %"arg_return!0", i8 %"arg1!0", i8 %"arg2!0", i8 %"arg3!0")
  store i8 %printf, ptr @"print_return!0", align 1
  %"arg_return!05" = load i8, ptr @"arg_return!0", align 1
  ret i8 %"arg_return!05"
}

declare noalias ptr @malloc(i32)

declare i64 @printf(ptr, ...)
