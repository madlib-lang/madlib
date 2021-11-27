; ModuleID = 'main'


 


define external ccc  {i8* (i8*, i8*)*, {}*}* @$Show$Number$show()    {
  ret {i8* (i8*, i8*)*, {}*}* @showNumber 
}


@$Show$Number =    global {{i8* (i8*, i8*)*, {}*}* ()*} { {i8* (i8*, i8*)*, {}*}* ()* @$Show$Number$show }


@$Functor$List$map =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @$Functor$List$map$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @$Functor$List$map$fn(i8*  %env_0, i8*  %f_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %f_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$4, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


@$Functor$List =    global {{i8* (i8*, i8*)*, {}*}*} { {i8* (i8*, i8*)*, {}*}* @$Functor$List$map }


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


define external ccc  i8* @$closureFn$0(i8*  %env_0, i8*  %l_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {i8*, i8*}* 
  %2 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %5 = load  i8*, i8** %4, align 8 
  %6 = bitcast i8* %3 to {i8*, i8*}* 
  %7 = bitcast i8* %l_0 to {i8*, i8*}* 
  %8 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %7)  
  %9 = and i1 %8, 1 
  br i1 %9, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %10 = bitcast {i8*, i8*}* %6 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %11 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %7)  
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = and i1 1, 1 
  %19 = and i1 %11, %18 
  br i1 %19, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %20 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  %21 = load  i8*, i8** %20, align 8 
  %22 = bitcast i8* %21 to {i8*, i8*}* 
  %23 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  %24 = load  i8*, i8** %23, align 8 
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 = bitcast {i8* (i8*, i8*)*, {}*}* @helper to {i8* (i8*, i8*)*, i8*}* 
  %27 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 0 
  %28 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %27, align 8 
  %29 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 =  call ccc  i8*  %28(i8*  %30, i8*  %5)  
  %32 = bitcast i8* %31 to {i8*, i8*}* 
  %33 = bitcast {i8*, i8*}* %22 to i8* 
  %34 = bitcast i8* %5 to {i8* (i8*, i8*)*, i8*}* 
  %35 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %34, i32 0, i32 0 
  %36 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %35, align 8 
  %37 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %34, i32 0, i32 1 
  %38 = load  i8*, i8** %37, align 8 
  %39 =  call ccc  i8*  %36(i8*  %38, i8*  %33)  
  %40 = bitcast i8* %39 to i8* 
  %41 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %40, {i8*, i8*}*  %6)  
  %42 = bitcast {i8*, i8*}* %41 to i8* 
  %43 = bitcast {i8*, i8*}* %32 to {i8* (i8*, i8*)*, i8*}* 
  %44 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %43, i32 0, i32 0 
  %45 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %44, align 8 
  %46 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %43, i32 0, i32 1 
  %47 = load  i8*, i8** %46, align 8 
  %48 =  call ccc  i8*  %45(i8*  %47, i8*  %42)  
  %49 = bitcast i8* %48 to i8* 
  %50 = bitcast {i8*, i8*}* %25 to i8* 
  %51 = bitcast i8* %49 to {i8* (i8*, i8*)*, i8*}* 
  %52 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %51, i32 0, i32 0 
  %53 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %52, align 8 
  %54 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %51, i32 0, i32 1 
  %55 = load  i8*, i8** %54, align 8 
  %56 = tail call ccc  i8*  %53(i8*  %55, i8*  %50)  
  br label %exitBlock_0 
exitBlock_0:
  %57 = phi i8* [%10, %branchExpBlock_0], [%56, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  ret i8* %57
}


define external ccc  i8* @$closureFn$1(i8*  %env_0, i8*  %acc_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %5 = bitcast {i8*, i8*}* %4 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i8*}* 
  %8 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  store  i8* %5, i8** %8, align 8 
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  store  i8* %3, i8** %9, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*}*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8* (i8*, i8*)*, {i8*, i8*}*}* 
  %12 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*}*}* %11, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$0, i8* (i8*, i8*)** %12, align 8 
  %13 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*}*}* %11, i32 0, i32 1 
  store  {i8*, i8*}* %7, {i8*, i8*}** %13, align 8 
  %14 = bitcast {i8* (i8*, i8*)*, {i8*, i8*}*}* %11 to i8* 
  ret i8* %14 
}


define external ccc  i8* @$closureFn$2(i8*  %env_0, i8*  %list_0)    {
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
  %12 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %11)  
  %13 = and i1 %12, 1 
  br i1 %13, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %14 = bitcast {i64}* @Nothing to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %15 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %11)  
  %16 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  %17 = load  i8*, i8** %16, align 8 
  %18 = bitcast i8* %17 to {i8*, i8*}* 
  %19 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  %20 = load  i8*, i8** %19, align 8 
  %21 = bitcast i8* %20 to {i8*, i8*}* 
  %22 = and i1 1, 1 
  %23 = and i1 %15, %22 
  br i1 %23, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %24 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 0 
  %25 = load  i8*, i8** %24, align 8 
  %26 = bitcast i8* %25 to {i8*, i8*}* 
  %27 = getelementptr  {i8*, i8*}, {i8*, i8*}* %11, i32 0, i32 1 
  %28 = load  i8*, i8** %27, align 8 
  %29 = bitcast i8* %28 to {i8*, i8*}* 
  %30 = fcmp oeq double %10, 0.000000e0 
  %31 = icmp eq i1 %30, 1 
  br i1 %31, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %32 = bitcast {i8*, i8*}* %26 to i8* 
  %33 = bitcast {i8* (i8*, i8*)*, {}*}* @Just to {i8* (i8*, i8*)*, i8*}* 
  %34 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %33, i32 0, i32 0 
  %35 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %34, align 8 
  %36 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %33, i32 0, i32 1 
  %37 = load  i8*, i8** %36, align 8 
  %38 =  call ccc  i8*  %35(i8*  %37, i8*  %32)  
  %39 = bitcast i8* %38 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %40 = fsub double %10, 1.000000e0 
  %41 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %42 = bitcast i8* %41 to double* 
  store  double %40, double* %42, align 8 
  %43 = bitcast double* %42 to i8* 
  %44 = bitcast {i8* (i8*, i8*)*, {}*}* @nth to {i8* (i8*, i8*)*, i8*}* 
  %45 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %44, i32 0, i32 0 
  %46 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %45, align 8 
  %47 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %44, i32 0, i32 1 
  %48 = load  i8*, i8** %47, align 8 
  %49 =  call ccc  i8*  %46(i8*  %48, i8*  %43)  
  %50 = bitcast i8* %49 to i8* 
  %51 = bitcast {i8*, i8*}* %29 to i8* 
  %52 = bitcast i8* %50 to {i8* (i8*, i8*)*, i8*}* 
  %53 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %52, i32 0, i32 0 
  %54 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %53, align 8 
  %55 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %52, i32 0, i32 1 
  %56 = load  i8*, i8** %55, align 8 
  %57 =  call ccc  i8*  %54(i8*  %56, i8*  %51)  
  %58 = bitcast i8* %57 to i8* 
  br label %condBlock_0 
condBlock_0:
  %59 = phi i8* [%39, %truthyBlock_0], [%58, %falsyBlock_0] 
  %60 = bitcast i8* %59 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %61 = phi i8* [%14, %branchExpBlock_0], [%60, %condBlock_0], [zeroinitializer, %nextBlock_0] 
  %62 = bitcast i8* %61 to i8* 
  ret i8* %62 
}


define external ccc  i8* @$closureFn$3(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 = fadd double %3, 1.000000e0 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %6 = bitcast i8* %5 to double* 
  store  double %4, double* %6, align 8 
  %7 = bitcast double* %6 to i8* 
  ret i8* %7 
}


define external ccc  i8* @$closureFn$4(i8*  %env_0, i8*  %m_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %m_0 to {i8*, i8*}* 
  %5 = bitcast {i8* (i8*, i8*)*, {}*}* @helper to {i8* (i8*, i8*)*, i8*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 0 
  %7 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %6, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %5, i32 0, i32 1 
  %9 = load  i8*, i8** %8, align 8 
  %10 =  call ccc  i8*  %7(i8*  %9, i8*  %3)  
  %11 = bitcast i8* %10 to {i8*, i8*}* 
  %12 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %13 = bitcast {i8*, i8*}* %11 to {i8* (i8*, i8*)*, i8*}* 
  %14 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %13, i32 0, i32 0 
  %15 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %14, align 8 
  %16 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %13, i32 0, i32 1 
  %17 = load  i8*, i8** %16, align 8 
  %18 =  call ccc  i8*  %15(i8*  %17, i8*  %12)  
  %19 = bitcast i8* %18 to {i8*, i8*}* 
  %20 = bitcast {i8*, i8*}* %4 to i8* 
  %21 = bitcast {i8*, i8*}* %19 to {i8* (i8*, i8*)*, i8*}* 
  %22 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %21, i32 0, i32 0 
  %23 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %22, align 8 
  %24 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %21, i32 0, i32 1 
  %25 = load  i8*, i8** %24, align 8 
  %26 =  call ccc  i8*  %23(i8*  %25, i8*  %20)  
  %27 = bitcast i8* %26 to {i8*, i8*}* 
  %28 = bitcast {i8*, i8*}* %27 to i8* 
  ret i8* %28 
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


@fn =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @fn$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @fn$fn(i8*  %env_0, i8*  %__0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %__0 to i8* 
  %3 = load  {i8*, i8*}*, {i8*, i8*}** @l1, align 8 
  %4 = bitcast {i8*, i8*}* %3 to i8* 
  ret i8* %4 
}


@helper =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @helper$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @helper$fn(i8*  %env_0, i8*  %f_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %f_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$1, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


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
  store  i8* (i8*, i8*)* @$closureFn$2, i8* (i8*, i8*)** %16, align 8 
  %17 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* %15, i32 0, i32 1 
  store  {i8*, i8*, i8*}* %10, {i8*, i8*, i8*}** %17, align 8 
  %18 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*}*}* %15 to i8* 
  ret i8* %18 
}


@getTop =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @getTop$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @getTop$fn(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to i8* 
  %3 = load  double, double* @top, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


@hop =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @hop$fn, {}* @$EMPTY_ENV }


@a =    global double undef


define external ccc  i8* @hop$fn(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to double* 
  %3 = load  double, double* %2, align 8 
  store  double %3, double* @a, align 8 
  %4 = fsub double %3, 1.000000e0 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %6 = bitcast i8* %5 to double* 
  store  double %4, double* %6, align 8 
  %7 = bitcast double* %6 to i8* 
  ret i8* %7 
}


@createBigList =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @createBigList$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @createBigList$fn(i8*  %env_0, i8*  %count_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %count_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 = fcmp oeq double %3, 0.000000e0 
  %5 = icmp eq i1 %4, 1 
  br i1 %5, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %6 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %7 = fsub double %3, 1.000000e0 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %9 = bitcast i8* %8 to double* 
  store  double %7, double* %9, align 8 
  %10 = bitcast double* %9 to i8* 
  %11 = bitcast {i8* (i8*, i8*)*, {}*}* @createBigList to {i8* (i8*, i8*)*, i8*}* 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %11, i32 0, i32 0 
  %13 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %12, align 8 
  %14 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %11, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 =  call ccc  i8*  %13(i8*  %15, i8*  %10)  
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %19 = bitcast i8* %18 to double* 
  store  double 9.000000e0, double* %19, align 8 
  %20 = bitcast double* %19 to i8* 
  %21 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %20, {i8*, i8*}*  %17)  
  %22 = bitcast {i8*, i8*}* %21 to i8* 
  br label %condBlock_0 
condBlock_0:
  %23 = phi i8* [%6, %truthyBlock_0], [%22, %falsyBlock_0] 
  %24 = bitcast i8* %23 to {i8*, i8*}* 
  %25 = bitcast {i8*, i8*}* %24 to i8* 
  ret i8* %25 
}


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i8* @MadList_length({i8*, i8*}*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @MadList_push(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@$EMPTY_ENV =    global {} {  }


@l1 =    global {i8*, i8*}* undef


@l2 =    global {i8*, i8*}* undef


@l3 =    global {i8*, i8*}* undef


@mapped =    global i8* undef


@top =    global double undef


@i =    global double undef


@j =    global double undef


@d =    global double undef


@r =    global double undef


@e =    global i8* undef


@big =    global {i8*, i8*}* undef


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %1 = bitcast i8* %0 to double* 
  store  double 3.000000e0, double* %1, align 8 
  %2 = bitcast double* %1 to i8* 
  %3 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %2)  
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double 2.000000e0, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %6, {i8*, i8*}*  %3)  
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %9 = bitcast i8* %8 to double* 
  store  double 1.000000e0, double* %9, align 8 
  %10 = bitcast double* %9 to i8* 
  %11 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %10, {i8*, i8*}*  %7)  
  store  {i8*, i8*}* %11, {i8*, i8*}** @l1, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %13 = bitcast i8* %12 to double* 
  store  double 6.000000e0, double* %13, align 8 
  %14 = bitcast double* %13 to i8* 
  %15 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %14)  
  %16 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %17 = bitcast i8* %16 to double* 
  store  double 5.000000e0, double* %17, align 8 
  %18 = bitcast double* %17 to i8* 
  %19 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %18, {i8*, i8*}*  %15)  
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %21 = bitcast i8* %20 to double* 
  store  double 4.000000e0, double* %21, align 8 
  %22 = bitcast double* %21 to i8* 
  %23 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %22, {i8*, i8*}*  %19)  
  store  {i8*, i8*}* %23, {i8*, i8*}** @l2, align 8 
  %24 =  call ccc  {i8*, i8*}*  @MadList_concat({i8*, i8*}*  %11, {i8*, i8*}*  %23)  
  store  {i8*, i8*}* %24, {i8*, i8*}** @l3, align 8 
  %25 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %26 = bitcast i8* %25 to double* 
  store  double 2.000000e0, double* %26, align 8 
  %27 = bitcast double* %26 to i8* 
  %28 = bitcast {i8* (i8*, i8*)*, {}*}* @fn to {i8* (i8*, i8*)*, i8*}* 
  %29 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %28, i32 0, i32 0 
  %30 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %29, align 8 
  %31 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %28, i32 0, i32 1 
  %32 = load  i8*, i8** %31, align 8 
  %33 =  call ccc  i8*  %30(i8*  %32, i8*  %27)  
  %34 = bitcast i8* %33 to {i8*, i8*}* 
  %35 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %34)  
  %36 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 0 
  %37 = load  i8*, i8** %36, align 8 
  %38 = bitcast i8* %37 to {i8*, i8*}* 
  %39 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 1 
  %40 = load  i8*, i8** %39, align 8 
  %41 = bitcast i8* %40 to {i8*, i8*}* 
  %42 = and i1 1, 1 
  %43 = and i1 %35, %42 
  br i1 %43, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %44 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 0 
  %45 = load  i8*, i8** %44, align 8 
  %46 = bitcast i8* %45 to {i8*, i8*}* 
  %47 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 1 
  %48 = load  i8*, i8** %47, align 8 
  %49 = bitcast i8* %48 to {i8*, i8*}* 
  %50 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %51 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %50, align 8 
  %52 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %51()  
  %53 = bitcast {i8*, i8*}* %46 to i8* 
  %54 = bitcast {i8* (i8*, i8*)*, {}*}* %52 to {i8* (i8*, i8*)*, i8*}* 
  %55 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %54, i32 0, i32 0 
  %56 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %55, align 8 
  %57 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %54, i32 0, i32 1 
  %58 = load  i8*, i8** %57, align 8 
  %59 =  call ccc  i8*  %56(i8*  %58, i8*  %53)  
  %60 = bitcast i8* %59 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %61 = phi i8* [%60, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  %62 = bitcast i8* %61 to i8* 
  %63 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %64 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %63, i32 0, i32 0 
  %65 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %64, align 8 
  %66 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %63, i32 0, i32 1 
  %67 = load  i8*, i8** %66, align 8 
  %68 =  call ccc  i8*  %65(i8*  %67, i8*  %62)  
  %69 = bitcast i8* %68 to i8* 
  %70 = getelementptr  {{i8* (i8*, i8*)*, {}*}*}, {{i8* (i8*, i8*)*, {}*}*}* @$Functor$List, i32 0, i32 0 
  %71 = load  {i8* (i8*, i8*)*, {}*}*, {i8* (i8*, i8*)*, {}*}** %70, align 8 
  %72 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %73 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %72, align 8 
  %74 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %73()  
  %75 = bitcast {i8* (i8*, i8*)*, {}*}* %74 to i8* 
  %76 = bitcast {i8* (i8*, i8*)*, {}*}* %71 to {i8* (i8*, i8*)*, i8*}* 
  %77 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %76, i32 0, i32 0 
  %78 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %77, align 8 
  %79 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %76, i32 0, i32 1 
  %80 = load  i8*, i8** %79, align 8 
  %81 =  call ccc  i8*  %78(i8*  %80, i8*  %75)  
  %82 = bitcast i8* %81 to i8* 
  %83 = bitcast {i8*, i8*}* %24 to i8* 
  %84 = bitcast i8* %82 to {i8* (i8*, i8*)*, i8*}* 
  %85 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %84, i32 0, i32 0 
  %86 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %85, align 8 
  %87 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %84, i32 0, i32 1 
  %88 = load  i8*, i8** %87, align 8 
  %89 =  call ccc  i8*  %86(i8*  %88, i8*  %83)  
  %90 = bitcast i8* %89 to {i8*, i8*}* 
  %91 =  call ccc  i1  @MadList_hasMinLength(double  3.000000e0, {i8*, i8*}*  %90)  
  %92 = getelementptr  {i8*, i8*}, {i8*, i8*}* %90, i32 0, i32 0 
  %93 = load  i8*, i8** %92, align 8 
  %94 = bitcast i8* %93 to {i8*, i8*}* 
  %95 = getelementptr  {i8*, i8*}, {i8*, i8*}* %90, i32 0, i32 1 
  %96 = load  i8*, i8** %95, align 8 
  %97 = bitcast i8* %96 to {i8*, i8*}* 
  %98 = getelementptr  {i8*, i8*}, {i8*, i8*}* %97, i32 0, i32 0 
  %99 = load  i8*, i8** %98, align 8 
  %100 = bitcast i8* %99 to {i8*, i8*}* 
  %101 = getelementptr  {i8*, i8*}, {i8*, i8*}* %97, i32 0, i32 1 
  %102 = load  i8*, i8** %101, align 8 
  %103 = bitcast i8* %102 to {i8*, i8*}* 
  %104 = getelementptr  {i8*, i8*}, {i8*, i8*}* %103, i32 0, i32 0 
  %105 = load  i8*, i8** %104, align 8 
  %106 = bitcast i8* %105 to {i8*, i8*}* 
  %107 = getelementptr  {i8*, i8*}, {i8*, i8*}* %103, i32 0, i32 1 
  %108 = load  i8*, i8** %107, align 8 
  %109 = bitcast i8* %108 to {i8*, i8*}* 
  %110 = and i1 1, 1 
  %111 = and i1 1, %110 
  %112 = and i1 1, %111 
  %113 = and i1 %91, %112 
  br i1 %113, label %branchExpBlock_1, label %exitBlock_1 
branchExpBlock_1:
  %114 = getelementptr  {i8*, i8*}, {i8*, i8*}* %90, i32 0, i32 0 
  %115 = load  i8*, i8** %114, align 8 
  %116 = bitcast i8* %115 to {i8*, i8*}* 
  %117 = getelementptr  {i8*, i8*}, {i8*, i8*}* %90, i32 0, i32 1 
  %118 = load  i8*, i8** %117, align 8 
  %119 = bitcast i8* %118 to {i8*, i8*}* 
  %120 = getelementptr  {i8*, i8*}, {i8*, i8*}* %119, i32 0, i32 0 
  %121 = load  i8*, i8** %120, align 8 
  %122 = bitcast i8* %121 to {i8*, i8*}* 
  %123 = getelementptr  {i8*, i8*}, {i8*, i8*}* %119, i32 0, i32 1 
  %124 = load  i8*, i8** %123, align 8 
  %125 = bitcast i8* %124 to {i8*, i8*}* 
  %126 = getelementptr  {i8*, i8*}, {i8*, i8*}* %125, i32 0, i32 0 
  %127 = load  i8*, i8** %126, align 8 
  %128 = bitcast i8* %127 to {i8*, i8*}* 
  %129 = getelementptr  {i8*, i8*}, {i8*, i8*}* %125, i32 0, i32 1 
  %130 = load  i8*, i8** %129, align 8 
  %131 = bitcast i8* %130 to {i8*, i8*}* 
  %132 = bitcast {i8*, i8*}* %128 to i8* 
  br label %exitBlock_1 
exitBlock_1:
  %133 = phi i8* [%132, %branchExpBlock_1], [zeroinitializer, %exitBlock_0] 
  %134 = bitcast i8* %133 to i8* 
  store  i8* %134, i8** @mapped, align 8 
  %135 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %136 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %135, i32 0, i32 0 
  %137 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %136, align 8 
  %138 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %135, i32 0, i32 1 
  %139 = load  i8*, i8** %138, align 8 
  %140 =  call ccc  i8*  %137(i8*  %139, i8*  %134)  
  %141 = bitcast i8* %140 to i8* 
  %142 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %143 = bitcast i8* %142 to double* 
  store  double 3.000000e0, double* %143, align 8 
  %144 = bitcast double* %143 to i8* 
  %145 = bitcast {i8* (i8*, i8*)*, {}*}* @nth to {i8* (i8*, i8*)*, i8*}* 
  %146 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %145, i32 0, i32 0 
  %147 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %146, align 8 
  %148 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %145, i32 0, i32 1 
  %149 = load  i8*, i8** %148, align 8 
  %150 =  call ccc  i8*  %147(i8*  %149, i8*  %144)  
  %151 = bitcast i8* %150 to i8* 
  %152 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %153 = bitcast i8* %152 to double* 
  store  double 5.000000e0, double* %153, align 8 
  %154 = bitcast double* %153 to i8* 
  %155 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %154)  
  %156 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %157 = bitcast i8* %156 to double* 
  store  double 4.000000e0, double* %157, align 8 
  %158 = bitcast double* %157 to i8* 
  %159 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %158, {i8*, i8*}*  %155)  
  %160 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %161 = bitcast i8* %160 to double* 
  store  double 3.000000e0, double* %161, align 8 
  %162 = bitcast double* %161 to i8* 
  %163 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %162, {i8*, i8*}*  %159)  
  %164 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %165 = bitcast i8* %164 to double* 
  store  double 2.000000e0, double* %165, align 8 
  %166 = bitcast double* %165 to i8* 
  %167 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %166, {i8*, i8*}*  %163)  
  %168 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %169 = bitcast i8* %168 to double* 
  store  double 1.000000e0, double* %169, align 8 
  %170 = bitcast double* %169 to i8* 
  %171 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %170, {i8*, i8*}*  %167)  
  %172 = bitcast {i8*, i8*}* %171 to i8* 
  %173 = bitcast i8* %151 to {i8* (i8*, i8*)*, i8*}* 
  %174 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %173, i32 0, i32 0 
  %175 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %174, align 8 
  %176 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %173, i32 0, i32 1 
  %177 = load  i8*, i8** %176, align 8 
  %178 =  call ccc  i8*  %175(i8*  %177, i8*  %172)  
  %179 = bitcast i8* %178 to i8* 
  %180 = bitcast i8* %179 to {i64, i8*}* 
  %181 = getelementptr  {i64, i8*}, {i64, i8*}* %180, i32 0, i32 1 
  %182 = getelementptr  {i64, i8*}, {i64, i8*}* %180, i32 0, i32 0 
  %183 = load  i64, i64* %182, align 8 
  %184 = icmp eq i64 0, %183 
  %185 = load  i8*, i8** %181, align 8 
  %186 = bitcast i8* %185 to i8* 
  %187 = and i1 1, 1 
  %188 = and i1 %184, %187 
  br i1 %188, label %branchExpBlock_2, label %nextBlock_0 
branchExpBlock_2:
  %189 = bitcast i8* %179 to {i64, i8*}* 
  %190 = getelementptr  {i64, i8*}, {i64, i8*}* %189, i32 0, i32 1 
  %191 = load  i8*, i8** %190, align 8 
  %192 = bitcast i8* %191 to i8* 
  %193 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %194 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %193, align 8 
  %195 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %194()  
  %196 = bitcast {i8* (i8*, i8*)*, {}*}* %195 to {i8* (i8*, i8*)*, i8*}* 
  %197 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %196, i32 0, i32 0 
  %198 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %197, align 8 
  %199 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %196, i32 0, i32 1 
  %200 = load  i8*, i8** %199, align 8 
  %201 =  call ccc  i8*  %198(i8*  %200, i8*  %192)  
  %202 = bitcast i8* %201 to i8* 
  %203 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %204 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %203, i32 0, i32 0 
  %205 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %204, align 8 
  %206 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %203, i32 0, i32 1 
  %207 = load  i8*, i8** %206, align 8 
  %208 =  call ccc  i8*  %205(i8*  %207, i8*  %202)  
  %209 = bitcast i8* %208 to i8* 
  br label %exitBlock_2 
nextBlock_0:
  %210 = bitcast i8* %179 to {i64}* 
  %211 = getelementptr  {i64}, {i64}* %210, i32 0, i32 0 
  %212 = load  i64, i64* %211, align 8 
  %213 = icmp eq i64 1, %212 
  %214 = and i1 %213, 1 
  br i1 %214, label %branchExpBlock_3, label %exitBlock_2 
branchExpBlock_3:
  %215 = bitcast i8* %179 to {i64}* 
  %216 =  call ccc  i8*  @GC_malloc(i64  8)  
  %217 = getelementptr  i8, i8* %216, i32 0 
  store  i8 110, i8* %217, align 8 
  %218 = getelementptr  i8, i8* %216, i32 1 
  store  i8 111, i8* %218, align 8 
  %219 = getelementptr  i8, i8* %216, i32 2 
  store  i8 116, i8* %219, align 8 
  %220 = getelementptr  i8, i8* %216, i32 3 
  store  i8 104, i8* %220, align 8 
  %221 = getelementptr  i8, i8* %216, i32 4 
  store  i8 105, i8* %221, align 8 
  %222 = getelementptr  i8, i8* %216, i32 5 
  store  i8 110, i8* %222, align 8 
  %223 = getelementptr  i8, i8* %216, i32 6 
  store  i8 103, i8* %223, align 8 
  %224 = getelementptr  i8, i8* %216, i32 7 
  store  i8 0, i8* %224, align 8 
  %225 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %226 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %225, i32 0, i32 0 
  %227 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %226, align 8 
  %228 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %225, i32 0, i32 1 
  %229 = load  i8*, i8** %228, align 8 
  %230 =  call ccc  i8*  %227(i8*  %229, i8*  %216)  
  %231 = bitcast i8* %230 to i8* 
  br label %exitBlock_2 
exitBlock_2:
  %232 = phi i8* [%209, %branchExpBlock_2], [%231, %branchExpBlock_3], [zeroinitializer, %nextBlock_0] 
  %233 = bitcast i8* %232 to i8* 
  store  double 1.900000e1, double* @top, align 8 
  %234 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %235 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %234, align 8 
  %236 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %235()  
  %237 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %238 = bitcast i8* %237 to double* 
  store  double 3.000000e0, double* %238, align 8 
  %239 = bitcast double* %238 to i8* 
  %240 = bitcast {i8* (i8*, i8*)*, {}*}* @getTop to {i8* (i8*, i8*)*, i8*}* 
  %241 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %240, i32 0, i32 0 
  %242 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %241, align 8 
  %243 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %240, i32 0, i32 1 
  %244 = load  i8*, i8** %243, align 8 
  %245 =  call ccc  i8*  %242(i8*  %244, i8*  %239)  
  %246 = bitcast i8* %245 to double* 
  %247 = load  double, double* %246, align 8 
  %248 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %249 = bitcast i8* %248 to double* 
  store  double %247, double* %249, align 8 
  %250 = bitcast double* %249 to i8* 
  %251 = bitcast {i8* (i8*, i8*)*, {}*}* %236 to {i8* (i8*, i8*)*, i8*}* 
  %252 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %251, i32 0, i32 0 
  %253 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %252, align 8 
  %254 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %251, i32 0, i32 1 
  %255 = load  i8*, i8** %254, align 8 
  %256 =  call ccc  i8*  %253(i8*  %255, i8*  %250)  
  %257 = bitcast i8* %256 to i8* 
  %258 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %259 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %258, i32 0, i32 0 
  %260 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %259, align 8 
  %261 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %258, i32 0, i32 1 
  %262 = load  i8*, i8** %261, align 8 
  %263 =  call ccc  i8*  %260(i8*  %262, i8*  %257)  
  %264 = bitcast i8* %263 to i8* 
  store  double 2.000000e0, double* @i, align 8 
  store  double 4.000000e0, double* @j, align 8 
  store  double 9.000000e0, double* @d, align 8 
  %265 = fmul double 4.000000e0, 9.000000e0 
  %266 = fadd double 2.000000e0, %265 
  store  double %266, double* @r, align 8 
  %267 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %268 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %267, align 8 
  %269 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %268()  
  %270 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %271 = bitcast i8* %270 to double* 
  store  double %266, double* %271, align 8 
  %272 = bitcast double* %271 to i8* 
  %273 = bitcast {i8* (i8*, i8*)*, {}*}* %269 to {i8* (i8*, i8*)*, i8*}* 
  %274 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %273, i32 0, i32 0 
  %275 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %274, align 8 
  %276 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %273, i32 0, i32 1 
  %277 = load  i8*, i8** %276, align 8 
  %278 =  call ccc  i8*  %275(i8*  %277, i8*  %272)  
  %279 = bitcast i8* %278 to i8* 
  store  i8* %279, i8** @e, align 8 
  %280 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %281 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %280, i32 0, i32 0 
  %282 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %281, align 8 
  %283 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %280, i32 0, i32 1 
  %284 = load  i8*, i8** %283, align 8 
  %285 =  call ccc  i8*  %282(i8*  %284, i8*  %279)  
  %286 = bitcast i8* %285 to i8* 
  %287 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %288 = bitcast i8* %287 to double* 
  store  double 1.000000e7, double* %288, align 8 
  %289 = bitcast double* %288 to i8* 
  %290 = bitcast {i8* (i8*, i8*)*, {}*}* @createBigList to {i8* (i8*, i8*)*, i8*}* 
  %291 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %290, i32 0, i32 0 
  %292 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %291, align 8 
  %293 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %290, i32 0, i32 1 
  %294 = load  i8*, i8** %293, align 8 
  %295 =  call ccc  i8*  %292(i8*  %294, i8*  %289)  
  %296 = bitcast i8* %295 to {i8*, i8*}* 
  store  {i8*, i8*}* %296, {i8*, i8*}** @big, align 8 
  %297 =  call ccc  i8*  @GC_malloc(i64  8)  
  %298 = getelementptr  i8, i8* %297, i32 0 
  store  i8 99, i8* %298, align 8 
  %299 = getelementptr  i8, i8* %297, i32 1 
  store  i8 114, i8* %299, align 8 
  %300 = getelementptr  i8, i8* %297, i32 2 
  store  i8 101, i8* %300, align 8 
  %301 = getelementptr  i8, i8* %297, i32 3 
  store  i8 97, i8* %301, align 8 
  %302 = getelementptr  i8, i8* %297, i32 4 
  store  i8 116, i8* %302, align 8 
  %303 = getelementptr  i8, i8* %297, i32 5 
  store  i8 101, i8* %303, align 8 
  %304 = getelementptr  i8, i8* %297, i32 6 
  store  i8 100, i8* %304, align 8 
  %305 = getelementptr  i8, i8* %297, i32 7 
  store  i8 0, i8* %305, align 8 
  %306 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %307 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %306, i32 0, i32 0 
  %308 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %307, align 8 
  %309 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %306, i32 0, i32 1 
  %310 = load  i8*, i8** %309, align 8 
  %311 =  call ccc  i8*  %308(i8*  %310, i8*  %297)  
  %312 = bitcast i8* %311 to i8* 
  %313 = getelementptr  {{i8* (i8*, i8*)*, {}*}*}, {{i8* (i8*, i8*)*, {}*}*}* @$Functor$List, i32 0, i32 0 
  %314 = load  {i8* (i8*, i8*)*, {}*}*, {i8* (i8*, i8*)*, {}*}** %313, align 8 
  %315 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %316 = bitcast i8* %315 to {}* 
  %317 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %318 = bitcast i8* %317 to {i8* (i8*, i8*)*, {}*}* 
  %319 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %318, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$3, i8* (i8*, i8*)** %319, align 8 
  %320 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %318, i32 0, i32 1 
  store  {}* %316, {}** %320, align 8 
  %321 = bitcast {i8* (i8*, i8*)*, {}*}* %318 to i8* 
  %322 = bitcast {i8* (i8*, i8*)*, {}*}* %314 to {i8* (i8*, i8*)*, i8*}* 
  %323 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %322, i32 0, i32 0 
  %324 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %323, align 8 
  %325 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %322, i32 0, i32 1 
  %326 = load  i8*, i8** %325, align 8 
  %327 =  call ccc  i8*  %324(i8*  %326, i8*  %321)  
  %328 = bitcast i8* %327 to i8* 
  %329 = bitcast {i8*, i8*}* %296 to i8* 
  %330 = bitcast i8* %328 to {i8* (i8*, i8*)*, i8*}* 
  %331 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %330, i32 0, i32 0 
  %332 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %331, align 8 
  %333 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %330, i32 0, i32 1 
  %334 = load  i8*, i8** %333, align 8 
  %335 =  call ccc  i8*  %332(i8*  %334, i8*  %329)  
  %336 = bitcast i8* %335 to {i8*, i8*}* 
  ret void 
}