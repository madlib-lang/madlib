; ModuleID = 'main'


 


define external ccc  i8* @$Show$Number$show(i8* )    {
  %2 = bitcast i8* (i8*)* @showNumber to i8* 
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


define external ccc  i8* @$Show$List$show(i8*  %$Show$g6_0)    {
  %1 = bitcast i8* %$Show$g6_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @anonymous$lifted$1 to i8* 
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


define external ccc  i8* @anonymous$lifted$0(i8*  %$Show$c28_0, i8*  %list_0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$c28_0 to i8* 
  %2 = bitcast i8* %list_0 to {i8*, i8*}* 
  %3 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %2)  
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %5 = load  i8*, i8** %4, align 8 
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %8 = load  i8*, i8** %7, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 1, 1 
  %11 = and i1 %3, %10 
  br i1 %11, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
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
  br label %exitBlock_0 
exitBlock_0:
  %30 = phi i8* [%29, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %30 
}


define external ccc  i8* @anonymous$lifted$1(i8*  %$Show$g6_0, i8*  %list_0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$g6_0 to i8* 
  %2 = bitcast i8* %list_0 to {i8*, i8*}* 
  %3 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*}*  %2)  
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %5 = load  i8*, i8** %4, align 8 
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %8 = load  i8*, i8** %7, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %9, i32 0, i32 0 
  %11 = load  i8*, i8** %10, align 8 
  %12 = bitcast i8* %11 to {i8*, i8*}* 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %9, i32 0, i32 1 
  %14 = load  i8*, i8** %13, align 8 
  %15 = bitcast i8* %14 to {i8*, i8*}* 
  %16 = and i1 1, 1 
  %17 = and i1 1, %16 
  %18 = and i1 %3, %17 
  br i1 %18, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %19 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %20 = load  i8*, i8** %19, align 8 
  %21 = bitcast i8* %20 to {i8*, i8*}* 
  %22 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %23 = load  i8*, i8** %22, align 8 
  %24 = bitcast i8* %23 to {i8*, i8*}* 
  %25 = getelementptr  {i8*, i8*}, {i8*, i8*}* %24, i32 0, i32 0 
  %26 = load  i8*, i8** %25, align 8 
  %27 = bitcast i8* %26 to {i8*, i8*}* 
  %28 = getelementptr  {i8*, i8*}, {i8*, i8*}* %24, i32 0, i32 1 
  %29 = load  i8*, i8** %28, align 8 
  %30 = bitcast i8* %29 to {i8*, i8*}* 
  %31 = bitcast i8* %1 to {i8*}* 
  %32 = getelementptr  {i8*}, {i8*}* %31, i32 0, i32 0 
  %33 = load  i8*, i8** %32, align 8 
  %34 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %35 = bitcast i8* %34 to {i8*, i32, i32}* 
  %36 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %35, i32 0, i32 0 
  store  i8* %33, i8** %36, align 8 
  %37 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %35, i32 0, i32 1 
  store  i32 1, i32* %37, align 8 
  %38 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %35, i32 0, i32 2 
  store  i32 1, i32* %38, align 8 
  %39 = bitcast {i8*, i32, i32}* %35 to i8* 
  %40 = bitcast {i8*, i8*}* %21 to i8* 
  %41 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %39, i32  1, i8*  %40)  
  %42 = bitcast i8* %41 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %43 =  call ccc  i1  @MadList_hasLength(double  1.000000e0, {i8*, i8*}*  %2)  
  %44 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %45 = load  i8*, i8** %44, align 8 
  %46 = bitcast i8* %45 to {i8*, i8*}* 
  %47 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %48 = load  i8*, i8** %47, align 8 
  %49 = bitcast i8* %48 to {i8*, i8*}* 
  %50 = and i1 1, 1 
  %51 = and i1 %43, %50 
  br i1 %51, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %52 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %53 = load  i8*, i8** %52, align 8 
  %54 = bitcast i8* %53 to {i8*, i8*}* 
  %55 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %56 = load  i8*, i8** %55, align 8 
  %57 = bitcast i8* %56 to {i8*, i8*}* 
  %58 = bitcast i8* %1 to {i8*}* 
  %59 = getelementptr  {i8*}, {i8*}* %58, i32 0, i32 0 
  %60 = load  i8*, i8** %59, align 8 
  %61 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %62 = bitcast i8* %61 to {i8*, i32, i32}* 
  %63 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %62, i32 0, i32 0 
  store  i8* %60, i8** %63, align 8 
  %64 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %62, i32 0, i32 1 
  store  i32 1, i32* %64, align 8 
  %65 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %62, i32 0, i32 2 
  store  i32 1, i32* %65, align 8 
  %66 = bitcast {i8*, i32, i32}* %62 to i8* 
  %67 = bitcast {i8*, i8*}* %54 to i8* 
  %68 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %66, i32  1, i8*  %67)  
  %69 = bitcast i8* %68 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %70 = phi i8* [%42, %branchExpBlock_0], [%69, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %70 
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


define external ccc  i8* @showList(i8*  %$Show$c28_0)    {
  %1 = bitcast i8* %$Show$c28_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @anonymous$lifted$0 to i8* 
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


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*)* @$Show$List$show to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32}* 
  %3 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 1 
  store  i32 1, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %2, i32 0, i32 2 
  store  i32 1, i32* %5, align 8 
  %6 = bitcast {i8*, i32, i32}* %2 to i8* 
  %7 = bitcast {i8* (i8*)*}* @$Show$Boolean to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %6, i32  1, i8*  %7)  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %12 = bitcast i8* %11 to i1* 
  store  i1 0, i1* %12, align 8 
  %13 = bitcast i1* %12 to i8* 
  %14 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %13)  
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %16 = bitcast i8* %15 to i1* 
  store  i1 1, i1* %16, align 8 
  %17 = bitcast i1* %16 to i8* 
  %18 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %17, {i8*, i8*}*  %14)  
  %19 = bitcast {i8*, i8*}* %18 to {i8*, i8*}* 
  %20 = bitcast {i8*, i8*}* %19 to i8* 
  %21 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %20)  
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
  %32 = bitcast {i8* (i8*)*}* @$Show$Number to i8* 
  %33 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %31, i32  1, i8*  %32)  
  %34 = bitcast i8* %33 to {i8*, i32, i32, i8*}* 
  %35 = bitcast {i8*, i32, i32, i8*}* %34 to i8* 
  %36 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %37 = bitcast i8* %36 to double* 
  store  double 2.130000e2, double* %37, align 8 
  %38 = bitcast double* %37 to i8* 
  %39 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %38)  
  %40 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %41 = bitcast i8* %40 to double* 
  store  double 1.980000e2, double* %41, align 8 
  %42 = bitcast double* %41 to i8* 
  %43 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %42, {i8*, i8*}*  %39)  
  %44 = bitcast {i8*, i8*}* %43 to {i8*, i8*}* 
  %45 = bitcast {i8*, i8*}* %44 to i8* 
  %46 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %35, i32  1, i8*  %45)  
  %47 = bitcast i8* %46 to i8* 
  %48 =  call ccc  i8*  @log(i8*  %47)  
  %49 = bitcast i8* %48 to i8* 
  %50 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %51 = bitcast i8* %50 to double* 
  store  double 3.000000e0, double* %51, align 8 
  %52 = bitcast double* %51 to i8* 
  %53 =  call ccc  i8*  @$Show$Number$show(i8*  %52)  
  %54 = bitcast i8* %53 to {i8*, i32, i32, i8*}* 
  %55 = bitcast {i8*, i32, i32, i8*}* %54 to i8* 
  %56 =  call ccc  i8*  @log(i8*  %55)  
  %57 = bitcast i8* %56 to i8* 
  %58 =  call ccc  i8*  @GC_malloc(i64  6)  
  %59 = getelementptr  i8, i8* %58, i32 0 
  store  i8 104, i8* %59, align 8 
  %60 = getelementptr  i8, i8* %58, i32 1 
  store  i8 101, i8* %60, align 8 
  %61 = getelementptr  i8, i8* %58, i32 2 
  store  i8 108, i8* %61, align 8 
  %62 = getelementptr  i8, i8* %58, i32 3 
  store  i8 108, i8* %62, align 8 
  %63 = getelementptr  i8, i8* %58, i32 4 
  store  i8 111, i8* %63, align 8 
  %64 = getelementptr  i8, i8* %58, i32 5 
  store  i8 0, i8* %64, align 8 
  %65 =  call ccc  i8*  @GC_malloc(i64  2)  
  %66 = getelementptr  i8, i8* %65, i32 0 
  store  i8 32, i8* %66, align 8 
  %67 = getelementptr  i8, i8* %65, i32 1 
  store  i8 0, i8* %67, align 8 
  %68 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %69 = bitcast i8* %68 to double* 
  store  double 3.000000e0, double* %69, align 8 
  %70 = bitcast double* %69 to i8* 
  %71 =  call ccc  i8*  @$Show$Number$show(i8*  %70)  
  %72 = bitcast i8* %71 to {i8*, i32, i32, i8*}* 
  %73 =  call ccc  i8*  @GC_malloc(i64  7)  
  %74 = getelementptr  i8, i8* %73, i32 0 
  store  i8 119, i8* %74, align 8 
  %75 = getelementptr  i8, i8* %73, i32 1 
  store  i8 111, i8* %75, align 8 
  %76 = getelementptr  i8, i8* %73, i32 2 
  store  i8 114, i8* %76, align 8 
  %77 = getelementptr  i8, i8* %73, i32 3 
  store  i8 108, i8* %77, align 8 
  %78 = getelementptr  i8, i8* %73, i32 4 
  store  i8 100, i8* %78, align 8 
  %79 = getelementptr  i8, i8* %73, i32 5 
  store  i8 33, i8* %79, align 8 
  %80 = getelementptr  i8, i8* %73, i32 6 
  store  i8 0, i8* %80, align 8 
  %81 =  call ccc  i8*  @__strConcat__({i8*, i32, i32, i8*}*  %72, i8*  %73)  
  %82 =  call ccc  i8*  @__strConcat__(i8*  %65, i8*  %81)  
  %83 =  call ccc  i8*  @__strConcat__(i8*  %58, i8*  %82)  
  %84 =  call ccc  i8*  @log(i8*  %83)  
  %85 = bitcast i8* %84 to i8* 
  ret void 
}