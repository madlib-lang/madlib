; ModuleID = 'main'


 


define external ccc  i8* @processItems$lifted$0(i8*  %with_0, i8*  %processed_0, i8*  %items_0)    {
; <label>:0:
  %1 = bitcast i8* %with_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %processed_0 to i8* 
  %3 = bitcast i8* %items_0 to {i8*, i8*}* 
  %4 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*}*  %3)  
  %5 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %6 = load  i8*, i8** %5, align 8 
  %7 = bitcast i8* %6 to i8* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %9 = load  i8*, i8** %8, align 8 
  %10 = bitcast i8* %9 to {i8*, i8*}* 
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %10, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = bitcast i8* %12 to i8* 
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
  %22 = bitcast i8* %21 to i8* 
  %23 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %24 = load  i8*, i8** %23, align 8 
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = getelementptr  {i8*, i8*}, {i8*, i8*}* %25, i32 0, i32 0 
  %27 = load  i8*, i8** %26, align 8 
  %28 = bitcast i8* %27 to i8* 
  %29 = getelementptr  {i8*, i8*}, {i8*, i8*}* %25, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 = bitcast i8* %30 to {i8*, i8*}* 
  %32 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %33 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %32, i32  1, i8*  %22)  
  %34 = bitcast i8* %33 to i8* 
  %35 =  call ccc  i8*  @GC_malloc(i64  3)  
  %36 = getelementptr  i8, i8* %35, i32 0 
  store  i8 44, i8* %36, align 8 
  %37 = getelementptr  i8, i8* %35, i32 1 
  store  i8 32, i8* %37, align 8 
  %38 = getelementptr  i8, i8* %35, i32 2 
  store  i8 0, i8* %38, align 8 
  %39 =  call ccc  i8*  @__strConcat__(i8*  %34, i8*  %35)  
  %40 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  %39)  
  %41 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %28, {i8*, i8*}*  %31)  
  %42 = bitcast {i8*, i8*}* %41 to {i8*, i8*}* 
  %43 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %44 = bitcast {i8*, i8*}* %42 to i8* 
  %45 =  call ccc  i8*  @processItems$lifted$0(i8*  %43, i8*  %40, i8*  %44)  
  %46 = bitcast i8* %45 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %47 =  call ccc  i1  @MadList_hasLength(double  1.000000e0, {i8*, i8*}*  %3)  
  %48 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %49 = load  i8*, i8** %48, align 8 
  %50 = bitcast i8* %49 to i8* 
  %51 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %52 = load  i8*, i8** %51, align 8 
  %53 = bitcast i8* %52 to {i8*, i8*}* 
  %54 = and i1 1, 1 
  %55 = and i1 %47, %54 
  br i1 %55, label %branchExpBlock_1, label %nextBlock_1 
branchExpBlock_1:
  %56 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 0 
  %57 = load  i8*, i8** %56, align 8 
  %58 = bitcast i8* %57 to i8* 
  %59 = getelementptr  {i8*, i8*}, {i8*, i8*}* %3, i32 0, i32 1 
  %60 = load  i8*, i8** %59, align 8 
  %61 = bitcast i8* %60 to {i8*, i8*}* 
  %62 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %63 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %62, i32  1, i8*  %58)  
  %64 = bitcast i8* %63 to i8* 
  %65 =  call ccc  i8*  @__strConcat__(i8*  %2, i8*  %64)  
  br label %exitBlock_0 
nextBlock_1:
  %66 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %3)  
  %67 = and i1 %66, 1 
  br i1 %67, label %branchExpBlock_2, label %exitBlock_0 
branchExpBlock_2:
  %68 =  call ccc  i8*  @GC_malloc(i64  1)  
  %69 = getelementptr  i8, i8* %68, i32 0 
  store  i8 0, i8* %69, align 8 
  br label %exitBlock_0 
exitBlock_0:
  %70 = phi i8* [%46, %branchExpBlock_0], [%65, %branchExpBlock_1], [%68, %branchExpBlock_2], [zeroinitializer, %nextBlock_1] 
  ret i8* %70 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @__70012456e9980aac31b023d264344232__log(i8* )    {
  %2 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @__70012456e9980aac31b023d264344232__showNumber(i8* )    {
  %2 =  call ccc  i8*  @__doubleToStr__(i8*  %0)  
  ret i8* %2 
}


define external ccc  i8* @__70012456e9980aac31b023d264344232__showWith(i8*  %with_0, i8*  %list_0)    {
  %1 = bitcast i8* %with_0 to {i8*, i32, i32, i8*}* 
  %2 = bitcast i8* %list_0 to {i8*, i8*}* 
  %3 = bitcast i8* (i8*, i8*, i8*)* @processItems$lifted$0 to i8* 
  %4 = bitcast {i8*, i32, i32, i8*}* %1 to i8* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %4, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8*, i32, i32, i8*}* 
  %10 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 0 
  store  i8* %3, i8** %10, align 8 
  %11 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 1 
  store  i32 3, i32* %11, align 8 
  %12 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 2 
  store  i32 2, i32* %12, align 8 
  %13 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %9, i32 0, i32 3 
  store  i8* %5, i8** %13, align 8 
  %14 =  call ccc  i8*  @GC_malloc(i64  2)  
  %15 = getelementptr  i8, i8* %14, i32 0 
  store  i8 91, i8* %15, align 8 
  %16 = getelementptr  i8, i8* %14, i32 1 
  store  i8 0, i8* %16, align 8 
  %17 = bitcast {i8*, i32, i32, i8*}* %9 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  1)  
  %19 = getelementptr  i8, i8* %18, i32 0 
  store  i8 0, i8* %19, align 8 
  %20 = bitcast {i8*, i8*}* %2 to i8* 
  %21 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %17, i32  2, i8*  %18, i8*  %20)  
  %22 = bitcast i8* %21 to i8* 
  %23 =  call ccc  i8*  @GC_malloc(i64  2)  
  %24 = getelementptr  i8, i8* %23, i32 0 
  store  i8 93, i8* %24, align 8 
  %25 = getelementptr  i8, i8* %23, i32 1 
  store  i8 0, i8* %25, align 8 
  %26 =  call ccc  i8*  @__strConcat__(i8*  %22, i8*  %23)  
  %27 =  call ccc  i8*  @__strConcat__(i8*  %14, i8*  %26)  
  ret i8* %27 
}


declare external ccc  i8* @__applyPAP__(i8*, i32, ...)    


declare external ccc  i8* @__buildRecord__(i32, i8*, ...)    


declare external ccc  i8* @__selectField__(i8*, i8*)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @__strConcat__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


define external ccc  void @main()    {
entry_0:
  %0 = bitcast i8* (i8*)* @__70012456e9980aac31b023d264344232__showNumber to i8* 
  %1 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %2 = bitcast i8* %1 to {i8*, i32, i32, i8*}* 
  %3 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 0 
  store  i8* %0, i8** %3, align 8 
  %4 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 1 
  store  i32 1, i32* %4, align 8 
  %5 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %2, i32 0, i32 2 
  store  i32 1, i32* %5, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double 3.000000e0, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  %9 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %8)  
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %11 = bitcast i8* %10 to double* 
  store  double 2.000000e0, double* %11, align 8 
  %12 = bitcast double* %11 to i8* 
  %13 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %12, {i8*, i8*}*  %9)  
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %15 = bitcast i8* %14 to double* 
  store  double 1.000000e0, double* %15, align 8 
  %16 = bitcast double* %15 to i8* 
  %17 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %16, {i8*, i8*}*  %13)  
  %18 = bitcast {i8*, i8*}* %17 to {i8*, i8*}* 
  %19 = bitcast {i8*, i32, i32, i8*}* %2 to i8* 
  %20 = bitcast {i8*, i8*}* %18 to i8* 
  %21 =  call ccc  i8*  @__70012456e9980aac31b023d264344232__showWith(i8*  %19, i8*  %20)  
  %22 = bitcast i8* %21 to i8* 
  %23 =  call ccc  i8*  @__70012456e9980aac31b023d264344232__log(i8*  %22)  
  %24 = bitcast i8* %23 to i8* 
  ret void 
}