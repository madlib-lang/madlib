; ModuleID = 'main'


 


define external ccc  {i8* (i8*, i8*)*, {}*}* @$Show$Number$show()    {
  ret {i8* (i8*, i8*)*, {}*}* @showNumber 
}


@$Show$Number =    global {{i8* (i8*, i8*)*, {}*}* ()*} { {i8* (i8*, i8*)*, {}*}* ()* @$Show$Number$show }


define external ccc  i8* @Just$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i64, i8*}* getelementptr inbounds ({i64, i8*}, {i64, i8*}* inttoptr (i32 0 to {i64, i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i64, i8*}* 
  %7 = getelementptr  {i64, i8*}, {i64, i8*}* %6, i32 0, i32 1 
  store  i8* %arg_0, i8** %7, align 8 
  %8 = getelementptr  {i64, i8*}, {i64, i8*}* %6, i32 0, i32 0 
  store  i64 0, i64* %8, align 8 
  %9 = bitcast {i64, i8*}* %6 to i8* 
  ret i8* %9 
}


@Just =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @Just$0, {}* zeroinitializer }


@Nothing =    global {i64} { i64 1 }


define external ccc  i8* @$closureFn$0(i8*  %env_0, i8*  %list_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {i8*, i8*, i8*}* 
  %2 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %1, i32 0, i32 1 
  %5 = load  i8*, i8** %4, align 8 
  %6 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %1, i32 0, i32 2 
  %7 = load  i8*, i8** %6, align 8 
  %8 = bitcast i8* %5 to i8* 
  %9 = bitcast i8* %7 to double* 
  %10 = load  double, double* %9, align 8 
  %11 = bitcast i8* %list_0 to {i8*, i8*}* 
  %12 =  call ccc  i8*  @MadList_length({i8*, i8*}*  %11)  
  %13 = bitcast i8* %12 to double* 
  %14 = load  double, double* %13, align 8 
  %15 = fcmp oeq double %14, 0.000000e0 
  %16 = and i1 %15, 1 
  br i1 %16, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %17 = bitcast {i64}* @Nothing to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %18 =  call ccc  i8*  @MadList_length({i8*, i8*}*  %11)  
  %19 = bitcast i8* %18 to double* 
  %20 = load  double, double* %19, align 8 
  %21 = fcmp oge double %20, 1.000000e0 
  %22 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  %23 = load  i8*, i8** %22, align 8 
  %24 = bitcast i8* %23 to {i8*, i8*}* 
  %25 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  %26 = load  i8*, i8** %25, align 8 
  %27 = bitcast i8* %26 to {i8*, i8*}* 
  %28 = and i1 1, 1 
  %29 = and i1 %21, %28 
  br i1 %29, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %30 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  %31 = load  i8*, i8** %30, align 8 
  %32 = bitcast i8* %31 to {i8*, i8*}* 
  %33 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  %34 = load  i8*, i8** %33, align 8 
  %35 = bitcast i8* %34 to {i8*, i8*}* 
  %36 = fcmp oeq double %10, 0.000000e0 
  %37 = icmp eq i1 %36, 1 
  br i1 %37, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %38 = bitcast {i8*, i8*}* %32 to i8* 
  %39 = bitcast {i8* (i8*, i8*)*, {}*}* @Just to {i8* (i8*, i8*)*, i8*}* 
  %40 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %39, i32 0, i32 0 
  %41 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %40, align 8 
  %42 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %39, i32 0, i32 1 
  %43 = load  i8*, i8** %42, align 8 
  %44 =  call ccc  i8*  %41(i8*  %43, i8*  %38)  
  %45 = bitcast i8* %44 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %46 = fsub double %10, 1.000000e0 
  %47 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %48 = bitcast i8* %47 to double* 
  store  double %46, double* %48, align 8 
  %49 = bitcast double* %48 to i8* 
  %50 = bitcast {i8* (i8*, i8*)*, {}*}* @nth to {i8* (i8*, i8*)*, i8*}* 
  %51 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %50, i32 0, i32 0 
  %52 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %51, align 8 
  %53 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %50, i32 0, i32 1 
  %54 = load  i8*, i8** %53, align 8 
  %55 =  call ccc  i8*  %52(i8*  %54, i8*  %49)  
  %56 = bitcast i8* %55 to i8* 
  %57 = bitcast {i8*, i8*}* %35 to i8* 
  %58 = bitcast i8* %56 to {i8* (i8*, i8*)*, i8*}* 
  %59 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %58, i32 0, i32 0 
  %60 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %59, align 8 
  %61 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %58, i32 0, i32 1 
  %62 = load  i8*, i8** %61, align 8 
  %63 =  call ccc  i8*  %60(i8*  %62, i8*  %57)  
  %64 = bitcast i8* %63 to i8* 
  br label %condBlock_0 
condBlock_0:
  %65 = phi i8* [%45, %truthyBlock_0], [%64, %falsyBlock_0] 
  %66 = bitcast i8* %65 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %67 = phi i8* [%17, %branchExpBlock_0], [%66, %condBlock_0], [zeroinitializer, %nextBlock_0] 
  %68 = bitcast i8* %67 to i8* 
  ret i8* %68 
}


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @log$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to i8* 
  %3 =  call ccc  i8*  @puts(i8*  %2)  
  ret i8* %3 
}


@log =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @log$0, {}* zeroinitializer }


declare external ccc  i8* @__doubleToStr__(double)    


define external ccc  i8* @showNumber$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @__doubleToStr__(double  %3)  
  ret i8* %4 
}


@showNumber =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @showNumber$0, {}* zeroinitializer }


@nth =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @nth$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @nth$fn(i8*  %env_0, i8*  %index_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %index_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 = bitcast {i8* (i8*, i8*)*, {}*}* @Just to i8* 
  %5 = bitcast {i64}* @Nothing to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double %3, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*}, {i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8*, i8*, i8*}* 
  %11 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %10, i32 0, i32 0 
  store  i8* %4, i8** %11, align 8 
  %12 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %10, i32 0, i32 1 
  store  i8* %5, i8** %12, align 8 
  %13 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %10, i32 0, i32 2 
  store  i8* %8, i8** %13, align 8 
  %14 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}*), i32 1) to i64))  
  %15 = bitcast i8* %14 to {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* 
  %16 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* %15, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$0, i8* (i8*, i8*)** %16, align 8 
  %17 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* %15, i32 0, i32 1 
  store  {i8*, i8*, i8*}* %10, {i8*, i8*, i8*}** %17, align 8 
  %18 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* %15 to i8* 
  ret i8* %18 
}


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @MadList_length({i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @MadList_push(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  ret void 
}