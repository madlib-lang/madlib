; ModuleID = 'main'


 


declare external ccc  i8* @__e9f54377746cb0755b8ab19b5a3e8824__Just(i8*)    


declare external ccc  i8* @__e9f54377746cb0755b8ab19b5a3e8824__Nothing()    


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log(i8* )    {
  %2 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %2 
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


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @__e9f54377746cb0755b8ab19b5a3e8824__Nothing()  
  %1 = bitcast i8* %0 to {i64}* 
  %2 = getelementptr  {i64}, {i64}* %1, i32 0, i32 0 
  %3 = load  i64, i64* %2, align 8 
  %4 = icmp eq i64 1, %3 
  %5 = and i1 %4, 1 
  br i1 %5, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %6 = bitcast i8* %0 to {i64}* 
  %7 =  call ccc  i8*  @GC_malloc(i64  5)  
  %8 = getelementptr  i8, i8* %7, i32 0 
  store  i8 110, i8* %8, align 8 
  %9 = getelementptr  i8, i8* %7, i32 1 
  store  i8 111, i8* %9, align 8 
  %10 = getelementptr  i8, i8* %7, i32 2 
  store  i8 112, i8* %10, align 8 
  %11 = getelementptr  i8, i8* %7, i32 3 
  store  i8 101, i8* %11, align 8 
  %12 = getelementptr  i8, i8* %7, i32 4 
  store  i8 0, i8* %12, align 8 
  %13 =  call ccc  i8*  @log(i8*  %7)  
  %14 = bitcast i8* %13 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %15 = bitcast i8* %0 to {i64, i8*}* 
  %16 = getelementptr  {i64, i8*}, {i64, i8*}* %15, i32 0, i32 1 
  %17 = getelementptr  {i64, i8*}, {i64, i8*}* %15, i32 0, i32 0 
  %18 = load  i64, i64* %17, align 8 
  %19 = icmp eq i64 0, %18 
  %20 = load  i8*, i8** %16, align 8 
  %21 = bitcast i8* %20 to i8* 
  %22 = and i1 1, 1 
  %23 = and i1 %19, %22 
  br i1 %23, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %24 = bitcast i8* %0 to {i64, i8*}* 
  %25 = getelementptr  {i64, i8*}, {i64, i8*}* %24, i32 0, i32 1 
  %26 = load  i8*, i8** %25, align 8 
  %27 = bitcast i8* %26 to i8* 
  %28 =  call ccc  i8*  @log(i8*  %27)  
  %29 = bitcast i8* %28 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %30 = phi i8* [%14, %branchExpBlock_0], [%29, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %31 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %32 = bitcast i8* %31 to double* 
  store  double 4.000000e0, double* %32, align 8 
  %33 = bitcast double* %32 to i8* 
  %34 =  call ccc  i8*  @__e9f54377746cb0755b8ab19b5a3e8824__Just(i8*  %33)  
  %35 = bitcast i8* %34 to i8* 
  %36 = bitcast i8* %35 to {i64, i8*}* 
  %37 = getelementptr  {i64, i8*}, {i64, i8*}* %36, i32 0, i32 1 
  %38 = getelementptr  {i64, i8*}, {i64, i8*}* %36, i32 0, i32 0 
  %39 = load  i64, i64* %38, align 8 
  %40 = icmp eq i64 0, %39 
  %41 = load  i8*, i8** %37, align 8 
  %42 = bitcast i8* %41 to i8* 
  %43 = and i1 1, 1 
  %44 = and i1 %40, %43 
  br i1 %44, label %branchExpBlock_2, label %exitBlock_1 
branchExpBlock_2:
  %45 = bitcast i8* %35 to {i64, i8*}* 
  %46 = getelementptr  {i64, i8*}, {i64, i8*}* %45, i32 0, i32 1 
  %47 = load  i8*, i8** %46, align 8 
  %48 = bitcast i8* %47 to i8* 
  %49 = fadd double %48, 1.000000e0 
  br label %exitBlock_1 
exitBlock_1:
  %50 = phi i8* [%49, %branchExpBlock_2], [zeroinitializer, %exitBlock_0] 
  ret void 
}