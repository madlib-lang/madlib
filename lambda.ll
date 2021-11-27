; ModuleID = 'main'


 


define external ccc  i8* @$closureFn$0(i8*  %env_0, i8*  %acc_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to double* 
  %5 = load  double, double* %4, align 8 
  %6 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %7 = fcmp oeq double %5, 0.000000e0 
  %8 = icmp eq i1 %7, 1 
  br i1 %8, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %9 = bitcast {i8*, i8*}* %6 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %10 = fsub double %5, 1.000000e0 
  %11 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %12 = bitcast i8* %11 to double* 
  store  double 9.000000e0, double* %12, align 8 
  %13 = bitcast double* %12 to i8* 
  %14 = bitcast {i8* (i8*, i8*)*, {}*}* @push to {i8* (i8*, i8*)*, i8*}* 
  %15 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %14, i32 0, i32 0 
  %16 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %15, align 8 
  %17 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %14, i32 0, i32 1 
  %18 = load  i8*, i8** %17, align 8 
  %19 =  call ccc  i8*  %16(i8*  %18, i8*  %13)  
  %20 = bitcast i8* %19 to {i8*, i8*}* 
  %21 = bitcast {i8*, i8*}* %6 to i8* 
  %22 = bitcast {i8*, i8*}* %20 to {i8* (i8*, i8*)*, i8*}* 
  %23 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %22, i32 0, i32 0 
  %24 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %23, align 8 
  %25 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %22, i32 0, i32 1 
  %26 = load  i8*, i8** %25, align 8 
  %27 =  call ccc  i8*  %24(i8*  %26, i8*  %21)  
  %28 = bitcast i8* %27 to {i8*, i8*}* 
  %29 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %30 = bitcast i8* %29 to double* 
  store  double %10, double* %30, align 8 
  %31 = bitcast double* %30 to i8* 
  %32 = bitcast {i8*, i8*}* %28 to i8* 
  %33 =  call ccc  i8*  @ch$uncurried(i8*  %31, i8*  %32)  
  %34 = bitcast i8* %33 to {i8*, i8*}* 
  br label %condBlock_0 
condBlock_0:
  %35 = phi i8* [%9, %truthyBlock_0], [%33, %falsyBlock_0] 
  %36 = bitcast i8* %35 to {i8*, i8*}* 
  %37 = bitcast {i8*, i8*}* %36 to i8* 
  ret i8* %37 
}


define external ccc  i8* @$closureFn$1(i8*  %env_0, i8*  %l_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {i8*, i8*}* 
  %2 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %1, i32 0, i32 1 
  %5 = load  i8*, i8** %4, align 8 
  %6 = bitcast i8* %3 to {i8*, i8*}* 
  %7 = bitcast i8* %l_0 to {i8*, i8*}* 
  %8 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %7)  
  %9 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  %10 = load  i8*, i8** %9, align 8 
  %11 = bitcast i8* %10 to {i8*, i8*}* 
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = and i1 1, 1 
  %16 = and i1 %8, %15 
  br i1 %16, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %17 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 0 
  %18 = load  i8*, i8** %17, align 8 
  %19 = bitcast i8* %18 to {i8*, i8*}* 
  %20 = getelementptr  {i8*, i8*}, {i8*, i8*}* %7, i32 0, i32 1 
  %21 = load  i8*, i8** %20, align 8 
  %22 = bitcast i8* %21 to {i8*, i8*}* 
  %23 = bitcast {i8*, i8*}* %19 to i8* 
  %24 = bitcast i8* %5 to {i8* (i8*, i8*)*, i8*}* 
  %25 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %24, i32 0, i32 0 
  %26 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %25, align 8 
  %27 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %24, i32 0, i32 1 
  %28 = load  i8*, i8** %27, align 8 
  %29 =  call ccc  i8*  %26(i8*  %28, i8*  %23)  
  %30 = bitcast i8* %29 to i8* 
  %31 = bitcast {i8* (i8*, i8*)*, {}*}* @push to {i8* (i8*, i8*)*, i8*}* 
  %32 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %31, i32 0, i32 0 
  %33 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %32, align 8 
  %34 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %31, i32 0, i32 1 
  %35 = load  i8*, i8** %34, align 8 
  %36 =  call ccc  i8*  %33(i8*  %35, i8*  %30)  
  %37 = bitcast i8* %36 to {i8*, i8*}* 
  %38 = bitcast {i8*, i8*}* %6 to i8* 
  %39 = bitcast {i8*, i8*}* %37 to {i8* (i8*, i8*)*, i8*}* 
  %40 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %39, i32 0, i32 0 
  %41 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %40, align 8 
  %42 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %39, i32 0, i32 1 
  %43 = load  i8*, i8** %42, align 8 
  %44 =  call ccc  i8*  %41(i8*  %43, i8*  %38)  
  %45 = bitcast i8* %44 to {i8*, i8*}* 
  %46 = bitcast {i8*, i8*}* %45 to i8* 
  %47 = bitcast {i8*, i8*}* %22 to i8* 
  %48 =  call ccc  i8*  @helper$uncurried(i8*  %5, i8*  %46, i8*  %47)  
  %49 = bitcast i8* %48 to {i8*, i8*}* 
  br label %exitBlock_0 
nextBlock_0:
  %50 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %7)  
  %51 = and i1 %50, 1 
  br i1 %51, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %52 = bitcast {i8*, i8*}* %6 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %53 = phi i8* [%48, %branchExpBlock_0], [%52, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %54 = bitcast i8* %53 to {i8*, i8*}* 
  %55 = bitcast {i8*, i8*}* %54 to i8* 
  ret i8* %55 
}


define external ccc  i8* @$closureFn$2(i8*  %env_0, i8*  %acc_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %5 = bitcast {i8*, i8*}* %4 to i8* 
  %6 = bitcast i8* %env_0 to {i8*, i8*, i8*, i8*, i8*}* 
  %7 = getelementptr  {i8*, i8*, i8*, i8*, i8*}, {i8*, i8*, i8*, i8*, i8*}* %6, i32 0, i32 0 
  store  i8* %5, i8** %7, align 8 
  %8 = getelementptr  {i8*, i8*, i8*, i8*, i8*}, {i8*, i8*, i8*, i8*, i8*}* %6, i32 0, i32 1 
  store  i8* %3, i8** %8, align 8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %10, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$1, i8* (i8*, i8*)** %11, align 8 
  %12 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %10, i32 0, i32 1 
  store  {i8*, i8*, i8*, i8*, i8*}* %6, {i8*, i8*, i8*, i8*, i8*}** %12, align 8 
  %13 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %10 to i8* 
  ret i8* %13 
}


define external ccc  i8* @$closureFn$3(i8*  %env_0, i8*  %m_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %m_0 to {i8*, i8*}* 
  %5 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %6 = bitcast {i8*, i8*}* %4 to i8* 
  %7 =  call ccc  i8*  @helper$uncurried(i8*  %3, i8*  %5, i8*  %6)  
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  ret i8* %7 
}


define external ccc  i8* @$closureFn$4(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  6)  
  %5 = getelementptr  i8, i8* %4, i32 0 
  store  i8 120, i8* %5, align 8 
  %6 = getelementptr  i8, i8* %4, i32 1 
  store  i8 32, i8* %6, align 8 
  %7 = getelementptr  i8, i8* %4, i32 2 
  store  i8 43, i8* %7, align 8 
  %8 = getelementptr  i8, i8* %4, i32 3 
  store  i8 32, i8* %8, align 8 
  %9 = getelementptr  i8, i8* %4, i32 4 
  store  i8 49, i8* %9, align 8 
  %10 = getelementptr  i8, i8* %4, i32 5 
  store  i8 0, i8* %10, align 8 
  ret i8* %4 
}


declare external ccc  i8* @MadList_map(i8*, i8*)    


define external ccc  i8* @map2$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @map2$1, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


define external ccc  i8* @map2$1(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %arg_0 to {i8*, i8*}* 
  %5 =  call ccc  i8*  @MadList_map(i8*  %3, {i8*, i8*}*  %4)  
  ret i8* %5 
}


@map2 =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @map2$0, {}* zeroinitializer }


declare external ccc  i8* @MadList_push(i8*, i8*)    


define external ccc  i8* @push$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %3 = bitcast i8* %2 to {i8*}* 
  %4 = getelementptr  {i8*}, {i8*}* %3, i32 0, i32 0 
  store  i8* %arg_0, i8** %4, align 8 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {i8*}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @push$1, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %6, i32 0, i32 1 
  store  {i8*}* %3, {i8*}** %8, align 8 
  %9 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %6 to i8* 
  ret i8* %9 
}


define external ccc  i8* @push$1(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to i8* 
  %5 = bitcast i8* %arg_0 to {i8*, i8*}* 
  %6 =  call ccc  i8*  @MadList_push(i8*  %4, {i8*, i8*}*  %5)  
  ret i8* %6 
}


@push =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @push$0, {}* zeroinitializer }


@ch =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @ch$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @ch$fn(i8*  %env_0, i8*  %count_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %count_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 = bitcast i8* %env_0 to {i8*, i8*, i8*, i8*, i8*}* 
  %8 = getelementptr  {i8*, i8*, i8*, i8*, i8*}, {i8*, i8*, i8*, i8*, i8*}* %7, i32 0, i32 0 
  store  i8* %6, i8** %8, align 8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}*), i32 1) to i64))  
  %10 = bitcast i8* %9 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %10, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$0, i8* (i8*, i8*)** %11, align 8 
  %12 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %10, i32 0, i32 1 
  store  {i8*, i8*, i8*, i8*, i8*}* %7, {i8*, i8*, i8*, i8*, i8*}** %12, align 8 
  %13 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %10 to i8* 
  ret i8* %13 
}


define external ccc  i8* @ch$uncurried(i8*  %count_0, i8*  %acc_0)    {
; <label>:0:
  %1 = bitcast i8* %count_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %4 = fcmp oeq double %2, 0.000000e0 
  %5 = icmp eq i1 %4, 1 
  br i1 %5, label %truthyBlock_0, label %falsyBlock_0 
truthyBlock_0:
  %6 = bitcast {i8*, i8*}* %3 to i8* 
  br label %condBlock_0 
falsyBlock_0:
  %7 = fsub double %2, 1.000000e0 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %9 = bitcast i8* %8 to double* 
  store  double 9.000000e0, double* %9, align 8 
  %10 = bitcast double* %9 to i8* 
  %11 = bitcast {i8* (i8*, i8*)*, {}*}* @push to {i8* (i8*, i8*)*, i8*}* 
  %12 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %11, i32 0, i32 0 
  %13 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %12, align 8 
  %14 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %11, i32 0, i32 1 
  %15 = load  i8*, i8** %14, align 8 
  %16 =  call ccc  i8*  %13(i8*  %15, i8*  %10)  
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast {i8*, i8*}* %3 to i8* 
  %19 = bitcast {i8*, i8*}* %17 to {i8* (i8*, i8*)*, i8*}* 
  %20 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %19, i32 0, i32 0 
  %21 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %20, align 8 
  %22 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %19, i32 0, i32 1 
  %23 = load  i8*, i8** %22, align 8 
  %24 =  call ccc  i8*  %21(i8*  %23, i8*  %18)  
  %25 = bitcast i8* %24 to {i8*, i8*}* 
  %26 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %27 = bitcast i8* %26 to double* 
  store  double %7, double* %27, align 8 
  %28 = bitcast double* %27 to i8* 
  %29 = bitcast {i8*, i8*}* %25 to i8* 
  %30 =  call ccc  i8*  @ch$uncurried(i8*  %28, i8*  %29)  
  %31 = bitcast i8* %30 to {i8*, i8*}* 
  br label %condBlock_0 
condBlock_0:
  %32 = phi i8* [%6, %truthyBlock_0], [%30, %falsyBlock_0] 
  %33 = bitcast i8* %32 to {i8*, i8*}* 
  %34 = bitcast {i8*, i8*}* %33 to i8* 
  ret i8* %34 
}


@createBigList =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @createBigList$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @createBigList$fn(i8*  %env_0, i8*  %count_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %count_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %8 =  call ccc  i8*  @ch$uncurried(i8*  %6, i8*  %7)  
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  ret i8* %8 
}


define external ccc  i8* @createBigList$uncurried(i8*  %count_0)    {
  %1 = bitcast i8* %count_0 to double* 
  %2 = load  double, double* %1, align 8 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %4 = bitcast i8* %3 to double* 
  store  double %2, double* %4, align 8 
  %5 = bitcast double* %4 to i8* 
  %6 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %7 =  call ccc  i8*  @ch$uncurried(i8*  %5, i8*  %6)  
  %8 = bitcast i8* %7 to {i8*, i8*}* 
  ret i8* %7 
}


@helper =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @helper$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @helper$fn(i8*  %env_0, i8*  %f_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %env_0 to {i8*, i8*, i8*, i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*, i8*, i8*, i8*}, {i8*, i8*, i8*, i8*, i8*}* %2, i32 0, i32 0 
  store  i8* %f_0, i8** %3, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$2, i8* (i8*, i8*)** %6, align 8 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %5, i32 0, i32 1 
  store  {i8*, i8*, i8*, i8*, i8*}* %2, {i8*, i8*, i8*, i8*, i8*}** %7, align 8 
  %8 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %5 to i8* 
  ret i8* %8 
}


define external ccc  i8* @helper$uncurried(i8*  %f_0, i8*  %acc_0, i8*  %l_0)    {
; <label>:0:
  %1 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %2 = bitcast i8* %l_0 to {i8*, i8*}* 
  %3 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %2)  
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %5 = load  i8*, i8** %4, align 8 
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %8 = load  i8*, i8** %7, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 1, 1 
  %11 = and i1 %3, %10 
  br i1 %11, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %12 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %13 = load  i8*, i8** %12, align 8 
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = bitcast {i8*, i8*}* %14 to i8* 
  %19 = bitcast i8* %f_0 to {i8* (i8*, i8*)*, i8*}* 
  %20 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %19, i32 0, i32 0 
  %21 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %20, align 8 
  %22 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %19, i32 0, i32 1 
  %23 = load  i8*, i8** %22, align 8 
  %24 =  call ccc  i8*  %21(i8*  %23, i8*  %18)  
  %25 = bitcast i8* %24 to i8* 
  %26 = bitcast {i8* (i8*, i8*)*, {}*}* @push to {i8* (i8*, i8*)*, i8*}* 
  %27 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 0 
  %28 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %27, align 8 
  %29 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 =  call ccc  i8*  %28(i8*  %30, i8*  %25)  
  %32 = bitcast i8* %31 to {i8*, i8*}* 
  %33 = bitcast {i8*, i8*}* %1 to i8* 
  %34 = bitcast {i8*, i8*}* %32 to {i8* (i8*, i8*)*, i8*}* 
  %35 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %34, i32 0, i32 0 
  %36 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %35, align 8 
  %37 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %34, i32 0, i32 1 
  %38 = load  i8*, i8** %37, align 8 
  %39 =  call ccc  i8*  %36(i8*  %38, i8*  %33)  
  %40 = bitcast i8* %39 to {i8*, i8*}* 
  %41 = bitcast {i8*, i8*}* %40 to i8* 
  %42 = bitcast {i8*, i8*}* %17 to i8* 
  %43 =  call ccc  i8*  @helper$uncurried(i8*  %f_0, i8*  %41, i8*  %42)  
  %44 = bitcast i8* %43 to {i8*, i8*}* 
  br label %exitBlock_0 
nextBlock_0:
  %45 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %2)  
  %46 = and i1 %45, 1 
  br i1 %46, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %47 = bitcast {i8*, i8*}* %1 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %48 = phi i8* [%43, %branchExpBlock_0], [%47, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %49 = bitcast i8* %48 to {i8*, i8*}* 
  %50 = bitcast {i8*, i8*}* %49 to i8* 
  ret i8* %50 
}


@map =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @map$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @map$fn(i8*  %env_0, i8*  %f_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %env_0 to {i8*, i8*, i8*, i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*, i8*, i8*, i8*}, {i8*, i8*, i8*, i8*, i8*}* %2, i32 0, i32 0 
  store  i8* %f_0, i8** %3, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* 
  %6 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %5, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$3, i8* (i8*, i8*)** %6, align 8 
  %7 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %5, i32 0, i32 1 
  store  {i8*, i8*, i8*, i8*, i8*}* %2, {i8*, i8*, i8*, i8*, i8*}** %7, align 8 
  %8 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %5 to i8* 
  ret i8* %8 
}


define external ccc  i8* @map$uncurried(i8*  %f_0, i8*  %m_0)    {
  %1 = bitcast i8* %m_0 to {i8*, i8*}* 
  %2 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %3 = bitcast {i8*, i8*}* %1 to i8* 
  %4 =  call ccc  i8*  @helper$uncurried(i8*  %f_0, i8*  %2, i8*  %3)  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  ret i8* %4 
}


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


declare external ccc  i1 @MadList_hasMinLength(double, {i8*, i8*}*)    


declare external ccc  i1 @MadList_hasLength(double, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_singleton(i8*)    


declare external ccc  {i8*, i8*}* @__MadList_push__(i8*, {i8*, i8*}*)    


declare external ccc  {i8*, i8*}* @MadList_concat({i8*, i8*}*, {i8*, i8*}*)    


@$EMPTY_ENV =    global {} {  }


@big =    global i8* undef


@result =    global i8* undef


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %1 = bitcast i8* %0 to double* 
  store  double 1.000000e7, double* %1, align 8 
  %2 = bitcast double* %1 to i8* 
  %3 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %4 =  call ccc  i8*  @ch$uncurried(i8*  %2, i8*  %3)  
  %5 = bitcast i8* %4 to {i8*, i8*}* 
  store  i8* %4, i8** @big, align 8 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*, i8*, i8*}, {i8*, i8*, i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*, i8*, i8*}*), i32 1) to i64))  
  %7 = bitcast i8* %6 to {i8*, i8*, i8*, i8*, i8*}* 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* 
  %10 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %9, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$4, i8* (i8*, i8*)** %10, align 8 
  %11 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %9, i32 0, i32 1 
  store  {i8*, i8*, i8*, i8*, i8*}* %7, {i8*, i8*, i8*, i8*, i8*}** %11, align 8 
  %12 = bitcast {i8* (i8*, i8*)*, {i8*, i8*, i8*, i8*, i8*}*}* %9 to i8* 
  %13 =  call ccc  i8*  @map$uncurried(i8*  %12, i8*  %4)  
  %14 = bitcast i8* %13 to {i8*, i8*}* 
  store  i8* %13, i8** @result, align 8 
  ret void 
}