; ModuleID = 'main'


 


define external ccc  i8* @$Show$Number$show(i8*  %n_0)    {
  %1 = bitcast i8* %n_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = load  {i8*, i32, i32, i8*}*, {i8*, i32, i32, i8*}** @yellow, align 8 
  %4 = bitcast {i8*, i32, i32, i8*}* %3 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %6 = bitcast i8* %5 to double* 
  store  double %2, double* %6, align 8 
  %7 = bitcast double* %6 to i8* 
  %8 =  call ccc  i8*  @showNumber(i8*  %7)  
  %9 = bitcast i8* %8 to i8* 
  %10 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %4, i32  1, i8*  %9)  
  %11 = bitcast i8* %10 to i8* 
  ret i8* %11 
}


@$Show$Number =    global {i8* (i8*)*} { i8* (i8*)* @$Show$Number$show }


define external ccc  i8* @$Show$Boolean$show(i8* )    {
  %2 = bitcast i8* (i8*)* @showBoolean to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*, i32, i32}* 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 0 
  store  i8* %2, i8** %5, align 8 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 1 
  store  i32 1, i32* %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %4, i32 0, i32 2 
  store  i32 1, i32* %7, align 8 
  %8 = bitcast {i8*, i32, i32}* %4 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  1, i8*  %0)  
  ret i8* %9 
}


@$Show$Boolean =    global {i8* (i8*)*} { i8* (i8*)* @$Show$Boolean$show }


@stringLiteral_0 =  unnamed_addr  constant [5 x i8] c"\22\\\22\22\00"


@stringLiteral_1 =  unnamed_addr  constant [5 x i8] c"\22\\\22\22\00"


define external ccc  i8* @$Show$String$show(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  i8*  @__strConcat__(i8*  %1, i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_1, i32 0, i32 0))  
  %3 =  call ccc  i8*  @__strConcat__(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_0, i32 0, i32 0), i8*  %2)  
  ret i8* %3 
}


@$Show$String =    global {i8* (i8*)*} { i8* (i8*)* @$Show$String$show }


define external ccc  i8* @$Show$List$show(i8*  %$Show$t19_0)    {
  %1 = bitcast i8* %$Show$t19_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @anonymous$lifted$2 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %1, i8** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %2, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 2, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 1, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 3 
  store  i8* %3, i8** %11, align 8 
  %12 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  ret i8* %12 
}


@$Show$List =    global {i8* (i8*)*} { i8* (i8*)* @$Show$List$show }


define external ccc  i8* @$Show$Tuple_2$show(i8*  %$Show$v73_0, i8*  %$Show$u72_0)    {
  %1 = bitcast i8* %$Show$v73_0 to i8* 
  %2 = bitcast i8* %$Show$u72_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$3 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 0 
  store  i8* %2, i8** %6, align 8 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 1 
  store  i8* %1, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 3, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 1, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %4, i8** %13, align 8 
  %14 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  ret i8* %14 
}


@$Show$Tuple_2 =    global {i8* (i8*, i8*)*} { i8* (i8*, i8*)* @$Show$Tuple_2$show }


@stringLiteral_0 =  unnamed_addr  constant [5 x i8] c"\22, \22\00"


@stringLiteral_1 =  unnamed_addr  constant [3 x i8] c"\22\22\00"


define external ccc  i8* @anonymous$lifted$1(i8*  %$Show$t19_0, i8*  %processed_0, i8*  %items_0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$t19_0 to i8* 
  %2 = bitcast i8* %processed_0 to i8* 
  %3 = bitcast i8* %items_0 to {i8*, i8*}* 
  %4 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*}*  %3)  
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8*, i8** %5, align 8 
  %7 = bitcast i8* %6 to {i8*, i8*}* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %9 = load  i8*, i8** %8, align 8 
  %10 = bitcast i8* %9 to {i8*, i8*}* 
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %10, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = bitcast i8* %12 to {i8*, i8*}* 
  %14 = getelementptr  {i8*, i8*}, {i8*, i8*}* %10, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 = bitcast i8* %15 to {i8*, i8*}* 
  %17 = and i1 1, 1 
  %18 = and i1 1, %17 
  %19 = and i1 %4, %18 
  br i1 %19, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %20 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %21 = load  i8*, i8** %20, align 8 
  %22 = bitcast i8* %21 to {i8*, i8*}* 
  %23 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %24 = load  i8*, i8** %23, align 8 
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = getelementptr  {i8*, i8*}, {i8*, i8*}* %25, i32 0, i32 0 
  %27 = load  i8*, i8** %26, align 8 
  %28 = bitcast i8* %27 to {i8*, i8*}* 
  %29 = getelementptr  {i8*, i8*}, {i8*, i8*}* %25, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 = bitcast i8* %30 to {i8*, i8*}* 
  %32 =  call ccc  i8*  @processItems$lifted$0(i8*  %1, i8*  %1)  
  %33 = bitcast i8* %1 to {i8*}* 
  %34 = getelementptr  {i8*}, {i8*}* %33, i32 0, i32 0 
  %35 = load  i8*, i8** %34, align 8 
  %36 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %37 = bitcast i8* %36 to {i8*, i32, i32}* 
  %38 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %37, i32 0, i32 0 
  store  i8* %35, i8** %38, align 8 
  %39 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %37, i32 0, i32 1 
  store  i32 1, i32* %39, align 8 
  %40 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %37, i32 0, i32 2 
  store  i32 1, i32* %40, align 8 
  %41 = bitcast {i8*, i32, i32}* %37 to i8* 
  %42 = bitcast {i8*, i8*}* %22 to i8* 
  %43 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %41, i32  1, i8*  %42)  
  %44 = bitcast i8* %43 to i8* 
  %45 =  call ccc  i8*  @__strConcat__(i8*  %44, i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_0, i32 0, i32 0))  
  %46 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  %45)  
  %47 = bitcast {i8*, i8*}* %28 to i8* 
  %48 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %47, {i8*, i8*}*  %31)  
  %49 = bitcast {i8*, i8*}* %48 to {i8*, i8*}* 
  %50 = bitcast {i8*, i8*}* %49 to i8* 
  %51 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %32, i32  2, i8*  %46, i8*  %50)  
  %52 = bitcast i8* %51 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %53 =  call ccc  i1  @MadList_hasLength(double  1.000000e0, {i8*, i8*}*  %3)  
  %54 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %55 = load  i8*, i8** %54, align 8 
  %56 = bitcast i8* %55 to {i8*, i8*}* 
  %57 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %58 = load  i8*, i8** %57, align 8 
  %59 = bitcast i8* %58 to {i8*, i8*}* 
  %60 = and i1 1, 1 
  %61 = and i1 %53, %60 
  br i1 %61, label %branchExpBlock_1, label %nextBlock_1 
branchExpBlock_1:
  %62 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %63 = load  i8*, i8** %62, align 8 
  %64 = bitcast i8* %63 to {i8*, i8*}* 
  %65 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %66 = load  i8*, i8** %65, align 8 
  %67 = bitcast i8* %66 to {i8*, i8*}* 
  %68 = bitcast i8* %1 to {i8*}* 
  %69 = getelementptr  {i8*}, {i8*}* %68, i32 0, i32 0 
  %70 = load  i8*, i8** %69, align 8 
  %71 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %72 = bitcast i8* %71 to {i8*, i32, i32}* 
  %73 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %72, i32 0, i32 0 
  store  i8* %70, i8** %73, align 8 
  %74 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %72, i32 0, i32 1 
  store  i32 1, i32* %74, align 8 
  %75 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %72, i32 0, i32 2 
  store  i32 1, i32* %75, align 8 
  %76 = bitcast {i8*, i32, i32}* %72 to i8* 
  %77 = bitcast {i8*, i8*}* %64 to i8* 
  %78 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %76, i32  1, i8*  %77)  
  %79 = bitcast i8* %78 to i8* 
  %80 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  %79)  
  br label %exitBlock_0 
nextBlock_1:
  br i1 1, label %branchExpBlock_2, label %exitBlock_0 
branchExpBlock_2:
  br label %exitBlock_0 
exitBlock_0:
  %81 = phi i8* [%52, %branchExpBlock_0], [%80, %branchExpBlock_1], [getelementptr inbounds ([3 x i8], [3 x i8]* @stringLiteral_1, i32 0, i32 0), %branchExpBlock_2], [zeroinitializer, %nextBlock_1] 
  ret i8* %81 
}


define external ccc  i8* @processItems$lifted$0(i8*  %$Show$t19_0, i8*  %$Show$t19_1)    {
  %1 = bitcast i8* %$Show$t19_0 to i8* 
  %2 = bitcast i8* %$Show$t19_1 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$1 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*}* 
  %6 = getelementptr  {i8*}, {i8*}* %5, i32 0, i32 0 
  store  i8* %2, i8** %6, align 8 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*, i32, i32, i8*}* 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 0 
  store  i8* %3, i8** %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 1 
  store  i32 3, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 2 
  store  i32 2, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %8, i32 0, i32 3 
  store  i8* %4, i8** %12, align 8 
  %13 = bitcast {i8*, i32, i32, i8*}* %8 to i8* 
  ret i8* %13 
}


@stringLiteral_0 =  unnamed_addr  constant [4 x i8] c"\22[\22\00"


@stringLiteral_1 =  unnamed_addr  constant [3 x i8] c"\22\22\00"


@stringLiteral_2 =  unnamed_addr  constant [4 x i8] c"\22]\22\00"


define external ccc  i8* @anonymous$lifted$2(i8*  %$Show$t19_0, i8*  %list_0)    {
  %1 = bitcast i8* %$Show$t19_0 to i8* 
  %2 = bitcast i8* %list_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*)* @processItems$lifted$0 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32}* 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 1 
  store  i32 2, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 2 
  store  i32 2, i32* %8, align 8 
  %9 = bitcast {i8*, i32, i32}* %5 to i8* 
  %10 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %9, i32  2, i8*  %1, i8*  %1)  
  %11 = bitcast i8* %10 to {i8*, i32, i32, i8*}* 
  %12 = bitcast {i8*, i32, i32, i8*}* %11 to i8* 
  %13 = bitcast {i8*, i8*}* %2 to i8* 
  %14 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %12, i32  2, i8*  getelementptr inbounds ([3 x i8], [3 x i8]* @stringLiteral_1, i32 0, i32 0), i8*  %13)  
  %15 = bitcast i8* %14 to i8* 
  %16 =  call ccc  i8*  @__strConcat__(i8*  %15, i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @stringLiteral_2, i32 0, i32 0))  
  %17 =  call ccc  i8*  @__strConcat__(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @stringLiteral_0, i32 0, i32 0), i8*  %16)  
  ret i8* %17 
}


@stringLiteral_0 =  unnamed_addr  constant [5 x i8] c"\22#[\22\00"


@stringLiteral_1 =  unnamed_addr  constant [5 x i8] c"\22, \22\00"


@stringLiteral_2 =  unnamed_addr  constant [4 x i8] c"\22]\22\00"


define external ccc  i8* @anonymous$lifted$3(i8*  %$Show$u72_0, i8*  %$Show$v73_0, i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$u72_0 to i8* 
  %2 = bitcast i8* %$Show$v73_0 to i8* 
  %3 = bitcast i8* %__x___0 to {i8*, i8*}* 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %6 = load  i8*, i8** %4, align 8 
  %7 = bitcast i8* %6 to {i8*, i8*}* 
  %8 = and i1 1, 1 
  %9 = load  i8*, i8** %5, align 8 
  %10 = bitcast i8* %9 to {i8*, i8*}* 
  %11 = and i1 %8, 1 
  br i1 %11, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast i8* %1 to {i8*}* 
  %19 = getelementptr  {i8*}, {i8*}* %18, i32 0, i32 0 
  %20 = load  i8*, i8** %19, align 8 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %22 = bitcast i8* %21 to {i8*, i32, i32}* 
  %23 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %22, i32 0, i32 0 
  store  i8* %20, i8** %23, align 8 
  %24 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %22, i32 0, i32 1 
  store  i32 1, i32* %24, align 8 
  %25 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %22, i32 0, i32 2 
  store  i32 1, i32* %25, align 8 
  %26 = bitcast {i8*, i32, i32}* %22 to i8* 
  %27 = bitcast {i8*, i8*}* %14 to i8* 
  %28 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %26, i32  1, i8*  %27)  
  %29 = bitcast i8* %28 to i8* 
  %30 = bitcast i8* %2 to {i8*}* 
  %31 = getelementptr  {i8*}, {i8*}* %30, i32 0, i32 0 
  %32 = load  i8*, i8** %31, align 8 
  %33 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %34 = bitcast i8* %33 to {i8*, i32, i32}* 
  %35 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %34, i32 0, i32 0 
  store  i8* %32, i8** %35, align 8 
  %36 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %34, i32 0, i32 1 
  store  i32 1, i32* %36, align 8 
  %37 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %34, i32 0, i32 2 
  store  i32 1, i32* %37, align 8 
  %38 = bitcast {i8*, i32, i32}* %34 to i8* 
  %39 = bitcast {i8*, i8*}* %17 to i8* 
  %40 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %38, i32  1, i8*  %39)  
  %41 = bitcast i8* %40 to i8* 
  %42 =  call ccc  i8*  @__strConcat__(i8*  %41, i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @stringLiteral_2, i32 0, i32 0))  
  %43 =  call ccc  i8*  @__strConcat__(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_1, i32 0, i32 0), i8*  %42)  
  %44 =  call ccc  i8*  @__strConcat__(i8*  %29, i8*  %43)  
  %45 =  call ccc  i8*  @__strConcat__(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_0, i32 0, i32 0), i8*  %44)  
  br label %exitBlock_0 
exitBlock_0:
  %46 = phi i8* [%45, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %46 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log(i8* )    {
  %2 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @showNumber(i8* )    {
  %2 =  call ccc  i8*  @__doubleToStr__(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @__booleanToStr__(i8*)    


define external ccc  i8* @showBoolean(i8* )    {
  %2 =  call ccc  i8*  @__booleanToStr__(i8*  %0)  
  ret i8* %2 
}


define external ccc  i8* @singleton(i8*  %a_0)    {
  %1 = bitcast i8* %a_0 to i8* 
  %2 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %1)  
  %3 = bitcast {i8*, i8*}* %2 to {i8*, i8*}* 
  %4 = bitcast {i8*, i8*}* %3 to i8* 
  ret i8* %4 
}


declare external ccc  i8* @MadList_append(i8*, i8*)    


define external ccc  i8* @append(i8* , i8* )    {
  %3 =  call ccc  i8*  @MadList_append(i8*  %0, i8*  %1)  
  ret i8* %3 
}


@stringLiteral_0 =  unnamed_addr  constant [10 x i8] c"\22\\x1b[0m\22\00"


define external ccc  i8* @withColor(i8*  %color_0, i8*  %v_0)    {
  %1 = bitcast i8* %color_0 to i8* 
  %2 = bitcast i8* %v_0 to i8* 
  %3 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  getelementptr inbounds ([10 x i8], [10 x i8]* @stringLiteral_0, i32 0, i32 0))  
  %4 =  call ccc  i8*  @__strConcat__(i8*  %1, i8*  %3)  
  ret i8* %4 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__strConcat__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@stringLiteral_0 =  unnamed_addr  constant [11 x i8] c"\22\\x1b[33m\22\00"


@yellow =    global {i8*, i32, i32, i8*}* undef


@stringLiteral_1 =  unnamed_addr  constant [11 x i8] c"\22\\x1b[90m\22\00"


@grey =    global {i8*, i32, i32, i8*}* undef


@stringLiteral_2 =  unnamed_addr  constant [5 x i8] c"\22ok\22\00"


@stringLiteral_3 =  unnamed_addr  constant [9 x i8] c"\22string\22\00"


@stringLiteral_4 =  unnamed_addr  constant [5 x i8] c"\22of\22\00"


@stringLiteral_5 =  unnamed_addr  constant [8 x i8] c"\22lists\22\00"


@stringLiteral_6 =  unnamed_addr  constant [8 x i8] c"\22shows\22\00"


@stringLiteral_7 =  unnamed_addr  constant [7 x i8] c"\22also\22\00"


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*, i8*)* @withColor to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*}* 
  %3 = getelementptr  {i8*}, {i8*}* %2, i32 0, i32 0 
  store  i8* getelementptr inbounds ([11 x i8], [11 x i8]* @stringLiteral_0, i32 0, i32 0), i8** %3, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32, i8*}* 
  %6 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 0 
  store  i8* %0, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 1 
  store  i32 2, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 2 
  store  i32 1, i32* %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %5, i32 0, i32 3 
  store  i8* %1, i8** %9, align 8 
  store  {i8*, i32, i32, i8*}* %5, {i8*, i32, i32, i8*}** @yellow, align 8 
  %10 = bitcast i8* (i8*, i8*)* @withColor to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %12 = bitcast i8* %11 to {i8*}* 
  %13 = getelementptr  {i8*}, {i8*}* %12, i32 0, i32 0 
  store  i8* getelementptr inbounds ([11 x i8], [11 x i8]* @stringLiteral_1, i32 0, i32 0), i8** %13, align 8 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i8*, i32, i32, i8*}* 
  %16 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 0 
  store  i8* %10, i8** %16, align 8 
  %17 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 1 
  store  i32 2, i32* %17, align 8 
  %18 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 2 
  store  i32 1, i32* %18, align 8 
  %19 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %15, i32 0, i32 3 
  store  i8* %11, i8** %19, align 8 
  store  {i8*, i32, i32, i8*}* %15, {i8*, i32, i32, i8*}** @grey, align 8 
  %20 = bitcast {i8*, i32, i32, i8*}* %5 to i8* 
  %21 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %20, i32  1, i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_2, i32 0, i32 0))  
  %22 = bitcast i8* %21 to i8* 
  %23 =  call ccc  i8*  @log(i8*  %22)  
  %24 = bitcast i8* %23 to i8* 
  %25 = bitcast i8* (i8*)* @$Show$List$show to i8* 
  %26 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %27 = bitcast i8* %26 to {i8*, i32, i32}* 
  %28 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %27, i32 0, i32 0 
  store  i8* %25, i8** %28, align 8 
  %29 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %27, i32 0, i32 1 
  store  i32 1, i32* %29, align 8 
  %30 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %27, i32 0, i32 2 
  store  i32 1, i32* %30, align 8 
  %31 = bitcast {i8*, i32, i32}* %27 to i8* 
  %32 = bitcast {i8* (i8*)*}* @$Show$Boolean to i8* 
  %33 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %31, i32  1, i8*  %32)  
  %34 = bitcast i8* %33 to {i8*, i32, i32, i8*}* 
  %35 = bitcast {i8*, i32, i32, i8*}* %34 to i8* 
  %36 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %37 = bitcast i8* %36 to i1* 
  store  i1 1, i1* %37, align 8 
  %38 = bitcast i1* %37 to i8* 
  %39 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %38)  
  %40 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %41 = bitcast i8* %40 to i1* 
  store  i1 0, i1* %41, align 8 
  %42 = bitcast i1* %41 to i8* 
  %43 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %42, {i8*, i8*}*  %39)  
  %44 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %45 = bitcast i8* %44 to i1* 
  store  i1 0, i1* %45, align 8 
  %46 = bitcast i1* %45 to i8* 
  %47 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %46, {i8*, i8*}*  %43)  
  %48 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %49 = bitcast i8* %48 to i1* 
  store  i1 0, i1* %49, align 8 
  %50 = bitcast i1* %49 to i8* 
  %51 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %50, {i8*, i8*}*  %47)  
  %52 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %53 = bitcast i8* %52 to i1* 
  store  i1 1, i1* %53, align 8 
  %54 = bitcast i1* %53 to i8* 
  %55 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %54, {i8*, i8*}*  %51)  
  %56 = bitcast {i8*, i8*}* %55 to {i8*, i8*}* 
  %57 = bitcast {i8*, i8*}* %56 to i8* 
  %58 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %35, i32  1, i8*  %57)  
  %59 = bitcast i8* %58 to i8* 
  %60 =  call ccc  i8*  @log(i8*  %59)  
  %61 = bitcast i8* %60 to i8* 
  %62 = bitcast i8* (i8*)* @$Show$List$show to i8* 
  %63 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %64 = bitcast i8* %63 to {i8*, i32, i32}* 
  %65 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %64, i32 0, i32 0 
  store  i8* %62, i8** %65, align 8 
  %66 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %64, i32 0, i32 1 
  store  i32 1, i32* %66, align 8 
  %67 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %64, i32 0, i32 2 
  store  i32 1, i32* %67, align 8 
  %68 = bitcast {i8*, i32, i32}* %64 to i8* 
  %69 = bitcast {i8* (i8*)*}* @$Show$Number to i8* 
  %70 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %68, i32  1, i8*  %69)  
  %71 = bitcast i8* %70 to {i8*, i32, i32, i8*}* 
  %72 = bitcast {i8*, i32, i32, i8*}* %71 to i8* 
  %73 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %74 = bitcast i8* %73 to double* 
  store  double 5.000000e0, double* %74, align 8 
  %75 = bitcast double* %74 to i8* 
  %76 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %75)  
  %77 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %78 = bitcast i8* %77 to double* 
  store  double 3.000000e0, double* %78, align 8 
  %79 = bitcast double* %78 to i8* 
  %80 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %79, {i8*, i8*}*  %76)  
  %81 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %82 = bitcast i8* %81 to double* 
  store  double 2.000000e0, double* %82, align 8 
  %83 = bitcast double* %82 to i8* 
  %84 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %83, {i8*, i8*}*  %80)  
  %85 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %86 = bitcast i8* %85 to double* 
  store  double 1.000000e0, double* %86, align 8 
  %87 = bitcast double* %86 to i8* 
  %88 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %87, {i8*, i8*}*  %84)  
  %89 = bitcast {i8*, i8*}* %88 to {i8*, i8*}* 
  %90 = bitcast {i8*, i8*}* %89 to i8* 
  %91 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %72, i32  1, i8*  %90)  
  %92 = bitcast i8* %91 to i8* 
  %93 =  call ccc  i8*  @log(i8*  %92)  
  %94 = bitcast i8* %93 to i8* 
  %95 = bitcast i8* (i8*)* @$Show$List$show to i8* 
  %96 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %97 = bitcast i8* %96 to {i8*, i32, i32}* 
  %98 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %97, i32 0, i32 0 
  store  i8* %95, i8** %98, align 8 
  %99 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %97, i32 0, i32 1 
  store  i32 1, i32* %99, align 8 
  %100 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %97, i32 0, i32 2 
  store  i32 1, i32* %100, align 8 
  %101 = bitcast {i8*, i32, i32}* %97 to i8* 
  %102 = bitcast {i8* (i8*)*}* @$Show$String to i8* 
  %103 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %101, i32  1, i8*  %102)  
  %104 = bitcast i8* %103 to {i8*, i32, i32, i8*}* 
  %105 = bitcast {i8*, i32, i32, i8*}* %104 to i8* 
  %106 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  getelementptr inbounds ([9 x i8], [9 x i8]* @stringLiteral_3, i32 0, i32 0))  
  %107 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  getelementptr inbounds ([5 x i8], [5 x i8]* @stringLiteral_4, i32 0, i32 0), {i8*, i8*}*  %106)  
  %108 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  getelementptr inbounds ([8 x i8], [8 x i8]* @stringLiteral_5, i32 0, i32 0), {i8*, i8*}*  %107)  
  %109 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  getelementptr inbounds ([8 x i8], [8 x i8]* @stringLiteral_6, i32 0, i32 0), {i8*, i8*}*  %108)  
  %110 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  getelementptr inbounds ([7 x i8], [7 x i8]* @stringLiteral_7, i32 0, i32 0), {i8*, i8*}*  %109)  
  %111 = bitcast {i8*, i8*}* %110 to {i8*, i8*}* 
  %112 = bitcast {i8*, i8*}* %111 to i8* 
  %113 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %105, i32  1, i8*  %112)  
  %114 = bitcast i8* %113 to i8* 
  %115 =  call ccc  i8*  @log(i8*  %114)  
  %116 = bitcast i8* %115 to i8* 
  %117 = bitcast i8* (i8*, i8*)* @$Show$Tuple_2$show to i8* 
  %118 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %119 = bitcast i8* %118 to {i8*, i32, i32}* 
  %120 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %119, i32 0, i32 0 
  store  i8* %117, i8** %120, align 8 
  %121 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %119, i32 0, i32 1 
  store  i32 2, i32* %121, align 8 
  %122 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %119, i32 0, i32 2 
  store  i32 2, i32* %122, align 8 
  %123 = bitcast {i8*, i32, i32}* %119 to i8* 
  %124 = bitcast {i8* (i8*)*}* @$Show$Number to i8* 
  %125 = bitcast {i8* (i8*)*}* @$Show$Boolean to i8* 
  %126 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %123, i32  2, i8*  %124, i8*  %125)  
  %127 = bitcast i8* %126 to {i8*, i32, i32, i8*}* 
  %128 = bitcast {i8*, i32, i32, i8*}* %127 to i8* 
  %129 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %130 = bitcast i8* %129 to i1* 
  store  i1 0, i1* %130, align 8 
  %131 = bitcast i1* %130 to i8* 
  %132 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %133 = bitcast i8* %132 to double* 
  store  double 1.230000e2, double* %133, align 8 
  %134 = bitcast double* %133 to i8* 
  %135 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %136 = bitcast i8* %135 to {i8*, i8*}* 
  %137 = getelementptr  {i8*, i8*}, {i8*, i8*}* %136, i32 0, i32 0 
  store  i8* %131, i8** %137, align 8 
  %138 = getelementptr  {i8*, i8*}, {i8*, i8*}* %136, i32 0, i32 1 
  store  i8* %134, i8** %138, align 8 
  %139 = bitcast {i8*, i8*}* %136 to i8* 
  %140 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %128, i32  1, i8*  %139)  
  %141 = bitcast i8* %140 to i8* 
  %142 =  call ccc  i8*  @log(i8*  %141)  
  %143 = bitcast i8* %142 to i8* 
  %144 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %145 = bitcast i8* %144 to double* 
  store  double 3.000000e0, double* %145, align 8 
  %146 = bitcast double* %145 to i8* 
  %147 =  call ccc  i8*  @$Show$Number$show(i8*  %146)  
  %148 = bitcast i8* %147 to i8* 
  %149 =  call ccc  i8*  @log(i8*  %148)  
  %150 = bitcast i8* %149 to i8* 
  %151 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %152 = bitcast i8* %151 to i1* 
  store  i1 1, i1* %152, align 8 
  %153 = bitcast i1* %152 to i8* 
  %154 =  call ccc  i8*  @$Show$Boolean$show(i8*  %153)  
  %155 = bitcast i8* %154 to i8* 
  %156 =  call ccc  i8*  @log(i8*  %155)  
  %157 = bitcast i8* %156 to i8* 
  ret void 
}