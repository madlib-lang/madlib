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


define external ccc  i8* @$Show$List$show(i8*  %$Show$j9_0)    {
  %1 = bitcast i8* %$Show$j9_0 to i8* 
  %2 = bitcast i8* (i8*, i8*)* @anonymous$lifted$3 to i8* 
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


define external ccc  i8* @$Show$Tuple_2$show(i8*  %$Show$k62_0, i8*  %$Show$l63_0)    {
  %1 = bitcast i8* %$Show$k62_0 to i8* 
  %2 = bitcast i8* %$Show$l63_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$4 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 0 
  store  i8* %1, i8** %6, align 8 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 1 
  store  i8* %2, i8** %7, align 8 
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


define external ccc  i8* @anonymous$lifted$0(i8*  %$Show$m90_0, i8*  %$Show$n91_0, i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$m90_0 to i8* 
  %2 = bitcast i8* %$Show$n91_0 to i8* 
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
  %18 =  call ccc  i8*  @GC_malloc(i64  3)  
  %19 = getelementptr  i8, i8* %18, i32 0 
  store  i8 35, i8* %19, align 8 
  %20 = getelementptr  i8, i8* %18, i32 1 
  store  i8 91, i8* %20, align 8 
  %21 = getelementptr  i8, i8* %18, i32 2 
  store  i8 0, i8* %21, align 8 
  %22 = bitcast i8* %1 to {i8*}* 
  %23 = getelementptr  {i8*}, {i8*}* %22, i32 0, i32 0 
  %24 = load  i8*, i8** %23, align 8 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %26 = bitcast i8* %25 to {i8*, i32, i32}* 
  %27 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %26, i32 0, i32 0 
  store  i8* %24, i8** %27, align 8 
  %28 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %26, i32 0, i32 1 
  store  i32 1, i32* %28, align 8 
  %29 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %26, i32 0, i32 2 
  store  i32 1, i32* %29, align 8 
  %30 = bitcast {i8*, i32, i32}* %26 to i8* 
  %31 = bitcast {i8*, i8*}* %14 to i8* 
  %32 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %30, i32  1, i8*  %31)  
  %33 = bitcast i8* %32 to i8* 
  %34 =  call ccc  i8*  @GC_malloc(i64  3)  
  %35 = getelementptr  i8, i8* %34, i32 0 
  store  i8 44, i8* %35, align 8 
  %36 = getelementptr  i8, i8* %34, i32 1 
  store  i8 32, i8* %36, align 8 
  %37 = getelementptr  i8, i8* %34, i32 2 
  store  i8 0, i8* %37, align 8 
  %38 = bitcast i8* %2 to {i8*}* 
  %39 = getelementptr  {i8*}, {i8*}* %38, i32 0, i32 0 
  %40 = load  i8*, i8** %39, align 8 
  %41 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %42 = bitcast i8* %41 to {i8*, i32, i32}* 
  %43 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %42, i32 0, i32 0 
  store  i8* %40, i8** %43, align 8 
  %44 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %42, i32 0, i32 1 
  store  i32 1, i32* %44, align 8 
  %45 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %42, i32 0, i32 2 
  store  i32 1, i32* %45, align 8 
  %46 = bitcast {i8*, i32, i32}* %42 to i8* 
  %47 = bitcast {i8*, i8*}* %17 to i8* 
  %48 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %46, i32  1, i8*  %47)  
  %49 = bitcast i8* %48 to i8* 
  %50 =  call ccc  i8*  @GC_malloc(i64  2)  
  %51 = getelementptr  i8, i8* %50, i32 0 
  store  i8 93, i8* %51, align 8 
  %52 = getelementptr  i8, i8* %50, i32 1 
  store  i8 0, i8* %52, align 8 
  %53 =  call ccc  i8*  @__strConcat__(i8*  %49, i8*  %50)  
  %54 =  call ccc  i8*  @__strConcat__(i8*  %34, i8*  %53)  
  %55 =  call ccc  i8*  @__strConcat__(i8*  %33, i8*  %54)  
  %56 =  call ccc  i8*  @__strConcat__(i8*  %18, i8*  %55)  
  br label %exitBlock_0 
exitBlock_0:
  %57 = phi i8* [%56, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %57 
}


define external ccc  i8* @anonymous$lifted$2(i8*  %$Show$j9_0, i8*  %processed_0, i8*  %items_0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$j9_0 to i8* 
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
  %32 =  call ccc  i8*  @processItems$lifted$1(i8*  %1)  
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
  %45 =  call ccc  i8*  @GC_malloc(i64  3)  
  %46 = getelementptr  i8, i8* %45, i32 0 
  store  i8 44, i8* %46, align 8 
  %47 = getelementptr  i8, i8* %45, i32 1 
  store  i8 32, i8* %47, align 8 
  %48 = getelementptr  i8, i8* %45, i32 2 
  store  i8 0, i8* %48, align 8 
  %49 =  call ccc  i8*  @__strConcat__(i8*  %44, i8*  %45)  
  %50 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  %49)  
  %51 = bitcast {i8*, i8*}* %28 to i8* 
  %52 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %51, {i8*, i8*}*  %31)  
  %53 = bitcast {i8*, i8*}* %52 to {i8*, i8*}* 
  %54 = bitcast {i8*, i8*}* %53 to i8* 
  %55 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %32, i32  2, i8*  %50, i8*  %54)  
  %56 = bitcast i8* %55 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %57 =  call ccc  i1  @MadList_hasLength(double  1.000000e0, {i8*, i8*}*  %3)  
  %58 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %59 = load  i8*, i8** %58, align 8 
  %60 = bitcast i8* %59 to {i8*, i8*}* 
  %61 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %62 = load  i8*, i8** %61, align 8 
  %63 = bitcast i8* %62 to {i8*, i8*}* 
  %64 = and i1 1, 1 
  %65 = and i1 %57, %64 
  br i1 %65, label %branchExpBlock_1, label %nextBlock_1 
branchExpBlock_1:
  %66 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %67 = load  i8*, i8** %66, align 8 
  %68 = bitcast i8* %67 to {i8*, i8*}* 
  %69 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %70 = load  i8*, i8** %69, align 8 
  %71 = bitcast i8* %70 to {i8*, i8*}* 
  %72 = bitcast i8* %1 to {i8*}* 
  %73 = getelementptr  {i8*}, {i8*}* %72, i32 0, i32 0 
  %74 = load  i8*, i8** %73, align 8 
  %75 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %76 = bitcast i8* %75 to {i8*, i32, i32}* 
  %77 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %76, i32 0, i32 0 
  store  i8* %74, i8** %77, align 8 
  %78 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %76, i32 0, i32 1 
  store  i32 1, i32* %78, align 8 
  %79 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %76, i32 0, i32 2 
  store  i32 1, i32* %79, align 8 
  %80 = bitcast {i8*, i32, i32}* %76 to i8* 
  %81 = bitcast {i8*, i8*}* %68 to i8* 
  %82 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %80, i32  1, i8*  %81)  
  %83 = bitcast i8* %82 to i8* 
  %84 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  %83)  
  br label %exitBlock_0 
nextBlock_1:
  br i1 1, label %branchExpBlock_2, label %exitBlock_0 
branchExpBlock_2:
  %85 =  call ccc  i8*  @GC_malloc(i64  1)  
  %86 = getelementptr  i8, i8* %85, i32 0 
  store  i8 0, i8* %86, align 8 
  br label %exitBlock_0 
exitBlock_0:
  %87 = phi i8* [%56, %branchExpBlock_0], [%84, %branchExpBlock_1], [%85, %branchExpBlock_2], [zeroinitializer, %nextBlock_1] 
  ret i8* %87 
}


define external ccc  i8* @processItems$lifted$1(i8*  %$Show$j9_0)    {
  %1 = bitcast i8* %$Show$j9_0 to i8* 
  %2 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$2 to i8* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {i8*}* 
  %5 = getelementptr  {i8*}, {i8*}* %4, i32 0, i32 0 
  store  i8* %1, i8** %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i32, i32, i8*}* 
  %8 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 0 
  store  i8* %2, i8** %8, align 8 
  %9 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 1 
  store  i32 3, i32* %9, align 8 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 2 
  store  i32 2, i32* %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %7, i32 0, i32 3 
  store  i8* %3, i8** %11, align 8 
  %12 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  ret i8* %12 
}


define external ccc  i8* @anonymous$lifted$3(i8*  %$Show$j9_0, i8*  %list_0)    {
  %1 = bitcast i8* %$Show$j9_0 to i8* 
  %2 = bitcast i8* %list_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*)* @processItems$lifted$1 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i32, i32}* 
  %6 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 1 
  store  i32 1, i32* %7, align 8 
  %8 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %5, i32 0, i32 2 
  store  i32 1, i32* %8, align 8 
  %9 =  call ccc  i8*  @GC_malloc(i64  2)  
  %10 = getelementptr  i8, i8* %9, i32 0 
  store  i8 91, i8* %10, align 8 
  %11 = getelementptr  i8, i8* %9, i32 1 
  store  i8 0, i8* %11, align 8 
  %12 = bitcast {i8*, i32, i32}* %5 to i8* 
  %13 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %12, i32  1, i8*  %1)  
  %14 = bitcast i8* %13 to {i8*, i32, i32, i8*}* 
  %15 = bitcast {i8*, i32, i32, i8*}* %14 to i8* 
  %16 =  call ccc  i8*  @GC_malloc(i64  1)  
  %17 = getelementptr  i8, i8* %16, i32 0 
  store  i8 0, i8* %17, align 8 
  %18 = bitcast {i8*, i8*}* %2 to i8* 
  %19 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %15, i32  2, i8*  %16, i8*  %18)  
  %20 = bitcast i8* %19 to i8* 
  %21 =  call ccc  i8*  @GC_malloc(i64  2)  
  %22 = getelementptr  i8, i8* %21, i32 0 
  store  i8 93, i8* %22, align 8 
  %23 = getelementptr  i8, i8* %21, i32 1 
  store  i8 0, i8* %23, align 8 
  %24 =  call ccc  i8*  @__strConcat__(i8*  %20, i8*  %21)  
  %25 =  call ccc  i8*  @__strConcat__(i8*  %9, i8*  %24)  
  ret i8* %25 
}


define external ccc  i8* @anonymous$lifted$4(i8*  %$Show$k62_0, i8*  %$Show$l63_0, i8*  %__x___0)    {
; <label>:0:
  %1 = bitcast i8* %$Show$k62_0 to i8* 
  %2 = bitcast i8* %$Show$l63_0 to i8* 
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
  %18 =  call ccc  i8*  @GC_malloc(i64  3)  
  %19 = getelementptr  i8, i8* %18, i32 0 
  store  i8 35, i8* %19, align 8 
  %20 = getelementptr  i8, i8* %18, i32 1 
  store  i8 91, i8* %20, align 8 
  %21 = getelementptr  i8, i8* %18, i32 2 
  store  i8 0, i8* %21, align 8 
  %22 = bitcast i8* %1 to {i8*}* 
  %23 = getelementptr  {i8*}, {i8*}* %22, i32 0, i32 0 
  %24 = load  i8*, i8** %23, align 8 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %26 = bitcast i8* %25 to {i8*, i32, i32}* 
  %27 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %26, i32 0, i32 0 
  store  i8* %24, i8** %27, align 8 
  %28 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %26, i32 0, i32 1 
  store  i32 1, i32* %28, align 8 
  %29 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %26, i32 0, i32 2 
  store  i32 1, i32* %29, align 8 
  %30 = bitcast {i8*, i32, i32}* %26 to i8* 
  %31 = bitcast {i8*, i8*}* %14 to i8* 
  %32 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %30, i32  1, i8*  %31)  
  %33 = bitcast i8* %32 to i8* 
  %34 =  call ccc  i8*  @GC_malloc(i64  3)  
  %35 = getelementptr  i8, i8* %34, i32 0 
  store  i8 44, i8* %35, align 8 
  %36 = getelementptr  i8, i8* %34, i32 1 
  store  i8 32, i8* %36, align 8 
  %37 = getelementptr  i8, i8* %34, i32 2 
  store  i8 0, i8* %37, align 8 
  %38 = bitcast i8* %2 to {i8*}* 
  %39 = getelementptr  {i8*}, {i8*}* %38, i32 0, i32 0 
  %40 = load  i8*, i8** %39, align 8 
  %41 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %42 = bitcast i8* %41 to {i8*, i32, i32}* 
  %43 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %42, i32 0, i32 0 
  store  i8* %40, i8** %43, align 8 
  %44 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %42, i32 0, i32 1 
  store  i32 1, i32* %44, align 8 
  %45 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %42, i32 0, i32 2 
  store  i32 1, i32* %45, align 8 
  %46 = bitcast {i8*, i32, i32}* %42 to i8* 
  %47 = bitcast {i8*, i8*}* %17 to i8* 
  %48 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %46, i32  1, i8*  %47)  
  %49 = bitcast i8* %48 to i8* 
  %50 =  call ccc  i8*  @GC_malloc(i64  2)  
  %51 = getelementptr  i8, i8* %50, i32 0 
  store  i8 93, i8* %51, align 8 
  %52 = getelementptr  i8, i8* %50, i32 1 
  store  i8 0, i8* %52, align 8 
  %53 =  call ccc  i8*  @__strConcat__(i8*  %49, i8*  %50)  
  %54 =  call ccc  i8*  @__strConcat__(i8*  %34, i8*  %53)  
  %55 =  call ccc  i8*  @__strConcat__(i8*  %33, i8*  %54)  
  %56 =  call ccc  i8*  @__strConcat__(i8*  %18, i8*  %55)  
  br label %exitBlock_0 
exitBlock_0:
  %57 = phi i8* [%56, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %57 
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


define external ccc  i8* @showTuple(i8*  %$Show$m90_0, i8*  %$Show$n91_0)    {
  %1 = bitcast i8* %$Show$m90_0 to i8* 
  %2 = bitcast i8* %$Show$n91_0 to i8* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @anonymous$lifted$0 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  %6 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 0 
  store  i8* %1, i8** %6, align 8 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 1 
  store  i8* %2, i8** %7, align 8 
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
  store  i1 0, i1* %16, align 8 
  %17 = bitcast i1* %16 to i8* 
  %18 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %17, {i8*, i8*}*  %14)  
  %19 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %20 = bitcast i8* %19 to i1* 
  store  i1 0, i1* %20, align 8 
  %21 = bitcast i1* %20 to i8* 
  %22 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %21, {i8*, i8*}*  %18)  
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %24 = bitcast i8* %23 to i1* 
  store  i1 1, i1* %24, align 8 
  %25 = bitcast i1* %24 to i8* 
  %26 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %25, {i8*, i8*}*  %22)  
  %27 = bitcast {i8*, i8*}* %26 to {i8*, i8*}* 
  %28 = bitcast {i8*, i8*}* %27 to i8* 
  %29 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %10, i32  1, i8*  %28)  
  %30 = bitcast i8* %29 to i8* 
  %31 =  call ccc  i8*  @log(i8*  %30)  
  %32 = bitcast i8* %31 to i8* 
  %33 = bitcast i8* (i8*, i8*)* @showTuple to i8* 
  %34 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %35 = bitcast i8* %34 to {i8*, i32, i32}* 
  %36 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %35, i32 0, i32 0 
  store  i8* %33, i8** %36, align 8 
  %37 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %35, i32 0, i32 1 
  store  i32 2, i32* %37, align 8 
  %38 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %35, i32 0, i32 2 
  store  i32 2, i32* %38, align 8 
  %39 = bitcast {i8*, i32, i32}* %35 to i8* 
  %40 = bitcast {i8* (i8*)*}* @$Show$Boolean to i8* 
  %41 = bitcast {i8* (i8*)*}* @$Show$Number to i8* 
  %42 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %39, i32  2, i8*  %40, i8*  %41)  
  %43 = bitcast i8* %42 to {i8*, i32, i32, i8*}* 
  %44 = bitcast {i8*, i32, i32, i8*}* %43 to i8* 
  %45 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %46 = bitcast i8* %45 to double* 
  store  double 1.230000e2, double* %46, align 8 
  %47 = bitcast double* %46 to i8* 
  %48 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i1* getelementptr inbounds (i1, i1* inttoptr (i32 0 to i1*), i32 1) to i64))  
  %49 = bitcast i8* %48 to i1* 
  store  i1 1, i1* %49, align 8 
  %50 = bitcast i1* %49 to i8* 
  %51 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %52 = bitcast i8* %51 to {i8*, i8*}* 
  %53 = getelementptr  {i8*, i8*}, {i8*, i8*}* %52, i32 0, i32 0 
  store  i8* %47, i8** %53, align 8 
  %54 = getelementptr  {i8*, i8*}, {i8*, i8*}* %52, i32 0, i32 1 
  store  i8* %50, i8** %54, align 8 
  %55 = bitcast {i8*, i8*}* %52 to i8* 
  %56 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %44, i32  1, i8*  %55)  
  %57 = bitcast i8* %56 to i8* 
  %58 =  call ccc  i8*  @log(i8*  %57)  
  %59 = bitcast i8* %58 to i8* 
  %60 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %61 = bitcast i8* %60 to double* 
  store  double 3.000000e0, double* %61, align 8 
  %62 = bitcast double* %61 to i8* 
  %63 =  call ccc  i8*  @$Show$Number$show(i8*  %62)  
  %64 = bitcast i8* %63 to i8* 
  %65 =  call ccc  i8*  @log(i8*  %64)  
  %66 = bitcast i8* %65 to i8* 
  %67 =  call ccc  i8*  @GC_malloc(i64  6)  
  %68 = getelementptr  i8, i8* %67, i32 0 
  store  i8 104, i8* %68, align 8 
  %69 = getelementptr  i8, i8* %67, i32 1 
  store  i8 101, i8* %69, align 8 
  %70 = getelementptr  i8, i8* %67, i32 2 
  store  i8 108, i8* %70, align 8 
  %71 = getelementptr  i8, i8* %67, i32 3 
  store  i8 108, i8* %71, align 8 
  %72 = getelementptr  i8, i8* %67, i32 4 
  store  i8 111, i8* %72, align 8 
  %73 = getelementptr  i8, i8* %67, i32 5 
  store  i8 0, i8* %73, align 8 
  %74 =  call ccc  i8*  @GC_malloc(i64  2)  
  %75 = getelementptr  i8, i8* %74, i32 0 
  store  i8 32, i8* %75, align 8 
  %76 = getelementptr  i8, i8* %74, i32 1 
  store  i8 0, i8* %76, align 8 
  %77 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %78 = bitcast i8* %77 to double* 
  store  double 3.000000e0, double* %78, align 8 
  %79 = bitcast double* %78 to i8* 
  %80 =  call ccc  i8*  @$Show$Number$show(i8*  %79)  
  %81 = bitcast i8* %80 to i8* 
  %82 =  call ccc  i8*  @GC_malloc(i64  7)  
  %83 = getelementptr  i8, i8* %82, i32 0 
  store  i8 119, i8* %83, align 8 
  %84 = getelementptr  i8, i8* %82, i32 1 
  store  i8 111, i8* %84, align 8 
  %85 = getelementptr  i8, i8* %82, i32 2 
  store  i8 114, i8* %85, align 8 
  %86 = getelementptr  i8, i8* %82, i32 3 
  store  i8 108, i8* %86, align 8 
  %87 = getelementptr  i8, i8* %82, i32 4 
  store  i8 100, i8* %87, align 8 
  %88 = getelementptr  i8, i8* %82, i32 5 
  store  i8 33, i8* %88, align 8 
  %89 = getelementptr  i8, i8* %82, i32 6 
  store  i8 0, i8* %89, align 8 
  %90 =  call ccc  i8*  @__strConcat__(i8*  %81, i8*  %82)  
  %91 =  call ccc  i8*  @__strConcat__(i8*  %74, i8*  %90)  
  %92 =  call ccc  i8*  @__strConcat__(i8*  %67, i8*  %91)  
  %93 =  call ccc  i8*  @log(i8*  %92)  
  %94 = bitcast i8* %93 to i8* 
  ret void 
}