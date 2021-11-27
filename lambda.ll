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
  store  i8* (i8*, i8*)* @$closureFn$9, i8* (i8*, i8*)** %7, align 8 
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


define external ccc  i8* @$closureFn$1(i8*  %env_0, i8*  %l_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %3 to {i8*, i8*}* 
  %5 = bitcast i8* %l_0 to {i8*, i8*}* 
  %6 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %5)  
  %7 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 0 
  %8 = load  i8*, i8** %7, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 1 
  %11 = load  i8*, i8** %10, align 8 
  %12 = bitcast i8* %11 to {i8*, i8*}* 
  %13 = and i1 1, 1 
  %14 = and i1 %6, %13 
  br i1 %14, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %15 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 0 
  %16 = load  i8*, i8** %15, align 8 
  %17 = bitcast i8* %16 to {i8*, i8*}* 
  %18 = getelementptr  {i8*, i8*}, {i8*, i8*}* %5, i32 0, i32 1 
  %19 = load  i8*, i8** %18, align 8 
  %20 = bitcast i8* %19 to {i8*, i8*}* 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %22 = bitcast i8* %21 to {}* 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {i8* (i8*, i8*)*, {}*}* 
  %25 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %24, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$helper$closureFn$0, i8* (i8*, i8*)** %25, align 8 
  %26 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %24, i32 0, i32 1 
  store  {}* %22, {}** %26, align 8 
  %27 = bitcast {i8*, i8*}* %17 to i8* 
  %28 = bitcast {i8* (i8*, i8*)*, {}*}* @push to {i8* (i8*, i8*)*, i8*}* 
  %29 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %28, i32 0, i32 0 
  %30 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %29, align 8 
  %31 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %28, i32 0, i32 1 
  %32 = load  i8*, i8** %31, align 8 
  %33 =  call ccc  i8*  %30(i8*  %32, i8*  %27)  
  %34 = bitcast i8* %33 to {i8*, i8*}* 
  %35 = bitcast {i8*, i8*}* %4 to i8* 
  %36 = bitcast {i8*, i8*}* %34 to {i8* (i8*, i8*)*, i8*}* 
  %37 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %36, i32 0, i32 0 
  %38 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %37, align 8 
  %39 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %36, i32 0, i32 1 
  %40 = load  i8*, i8** %39, align 8 
  %41 =  call ccc  i8*  %38(i8*  %40, i8*  %35)  
  %42 = bitcast i8* %41 to {i8*, i8*}* 
  %43 = bitcast {i8*, i8*}* %42 to i8* 
  %44 = bitcast {i8* (i8*, i8*)*, {}*}* %24 to {i8* (i8*, i8*)*, i8*}* 
  %45 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %44, i32 0, i32 0 
  %46 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %45, align 8 
  %47 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %44, i32 0, i32 1 
  %48 = load  i8*, i8** %47, align 8 
  %49 =  call ccc  i8*  %46(i8*  %48, i8*  %43)  
  %50 = bitcast i8* %49 to {i8*, i8*}* 
  %51 = bitcast {i8*, i8*}* %20 to i8* 
  %52 = bitcast {i8*, i8*}* %50 to {i8* (i8*, i8*)*, i8*}* 
  %53 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %52, i32 0, i32 0 
  %54 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %53, align 8 
  %55 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %52, i32 0, i32 1 
  %56 = load  i8*, i8** %55, align 8 
  %57 =  call ccc  i8*  %54(i8*  %56, i8*  %51)  
  %58 = bitcast i8* %57 to {i8*, i8*}* 
  %59 = bitcast {i8*, i8*}* %58 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %60 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %5)  
  %61 = and i1 %60, 1 
  br i1 %61, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %62 = bitcast {i8*, i8*}* %4 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %63 = phi i8* [%59, %branchExpBlock_0], [%62, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %64 = bitcast i8* %63 to {i8*, i8*}* 
  %65 = bitcast {i8*, i8*}* %64 to i8* 
  ret i8* %65 
}


define external ccc  i8* @$helper$closureFn$0(i8*  %env_0, i8*  %acc_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %acc_0 to {i8*, i8*}* 
  %3 = bitcast {i8*, i8*}* %2 to i8* 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {i8*}* 
  %6 = getelementptr  {i8*}, {i8*}* %5, i32 0, i32 0 
  store  i8* %3, i8** %6, align 8 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8* (i8*, i8*)*, {i8*}*}* 
  %9 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %8, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$1, i8* (i8*, i8*)** %9, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %8, i32 0, i32 1 
  store  {i8*}* %5, {i8*}** %10, align 8 
  %11 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %8 to i8* 
  ret i8* %11 
}


define external ccc  i8* @$closureFn$2(i8*  %env_0, i8*  %x_0)    {
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


define external ccc  i8* @$closureFn$3(i8*  %env_0, i8*  %list_0)    {
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


define external ccc  i8* @$inc$closureFn$4(i8*  %env_0, i8*  %x_0)    {
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


define external ccc  i8* @$closureFn$5(i8*  %env_0, i8*  %acc_0)    {
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
  store  double %10, double* %12, align 8 
  %13 = bitcast double* %12 to i8* 
  %14 = bitcast {i8* (i8*, i8*)*, {}*}* @ch to {i8* (i8*, i8*)*, i8*}* 
  %15 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %14, i32 0, i32 0 
  %16 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %15, align 8 
  %17 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %14, i32 0, i32 1 
  %18 = load  i8*, i8** %17, align 8 
  %19 =  call ccc  i8*  %16(i8*  %18, i8*  %13)  
  %20 = bitcast i8* %19 to {i8*, i8*}* 
  %21 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %22 = bitcast i8* %21 to double* 
  store  double 9.000000e0, double* %22, align 8 
  %23 = bitcast double* %22 to i8* 
  %24 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %23, {i8*, i8*}*  %6)  
  %25 = bitcast {i8*, i8*}* %24 to i8* 
  %26 = bitcast {i8*, i8*}* %20 to {i8* (i8*, i8*)*, i8*}* 
  %27 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 0 
  %28 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %27, align 8 
  %29 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %26, i32 0, i32 1 
  %30 = load  i8*, i8** %29, align 8 
  %31 =  call ccc  i8*  %28(i8*  %30, i8*  %25)  
  %32 = bitcast i8* %31 to {i8*, i8*}* 
  %33 = bitcast {i8*, i8*}* %32 to i8* 
  br label %condBlock_0 
condBlock_0:
  %34 = phi i8* [%9, %truthyBlock_0], [%33, %falsyBlock_0] 
  %35 = bitcast i8* %34 to {i8*, i8*}* 
  %36 = bitcast {i8*, i8*}* %35 to i8* 
  ret i8* %36 
}


define external ccc  i8* @$closureFn$6(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 = fmul double %3, 2.000000e0 
  %5 = fadd double %4, 1.000000e0 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %7 = bitcast i8* %6 to double* 
  store  double %5, double* %7, align 8 
  %8 = bitcast double* %7 to i8* 
  ret i8* %8 
}


define external ccc  i8* @$closureFn$8(i8*  %env_0, i8*  %l_0)    {
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
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {i8*}* 
  %25 = getelementptr  {i8*}, {i8*}* %24, i32 0, i32 0 
  store  i8* %5, i8** %25, align 8 
  %26 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %27 = bitcast i8* %26 to {i8* (i8*, i8*)*, {i8*}*}* 
  %28 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %27, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$helper$closureFn$7, i8* (i8*, i8*)** %28, align 8 
  %29 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %27, i32 0, i32 1 
  store  {i8*}* %24, {i8*}** %29, align 8 
  %30 = bitcast {i8*, i8*}* %19 to i8* 
  %31 = bitcast i8* %5 to {i8* (i8*, i8*)*, i8*}* 
  %32 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %31, i32 0, i32 0 
  %33 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %32, align 8 
  %34 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %31, i32 0, i32 1 
  %35 = load  i8*, i8** %34, align 8 
  %36 =  call ccc  i8*  %33(i8*  %35, i8*  %30)  
  %37 = bitcast i8* %36 to i8* 
  %38 = bitcast {i8* (i8*, i8*)*, {}*}* @push to {i8* (i8*, i8*)*, i8*}* 
  %39 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %38, i32 0, i32 0 
  %40 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %39, align 8 
  %41 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %38, i32 0, i32 1 
  %42 = load  i8*, i8** %41, align 8 
  %43 =  call ccc  i8*  %40(i8*  %42, i8*  %37)  
  %44 = bitcast i8* %43 to {i8*, i8*}* 
  %45 = bitcast {i8*, i8*}* %6 to i8* 
  %46 = bitcast {i8*, i8*}* %44 to {i8* (i8*, i8*)*, i8*}* 
  %47 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %46, i32 0, i32 0 
  %48 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %47, align 8 
  %49 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %46, i32 0, i32 1 
  %50 = load  i8*, i8** %49, align 8 
  %51 =  call ccc  i8*  %48(i8*  %50, i8*  %45)  
  %52 = bitcast i8* %51 to {i8*, i8*}* 
  %53 = bitcast {i8*, i8*}* %52 to i8* 
  %54 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %27 to {i8* (i8*, i8*)*, i8*}* 
  %55 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %54, i32 0, i32 0 
  %56 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %55, align 8 
  %57 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %54, i32 0, i32 1 
  %58 = load  i8*, i8** %57, align 8 
  %59 =  call ccc  i8*  %56(i8*  %58, i8*  %53)  
  %60 = bitcast i8* %59 to {i8*, i8*}* 
  %61 = bitcast {i8*, i8*}* %22 to i8* 
  %62 = bitcast {i8*, i8*}* %60 to {i8* (i8*, i8*)*, i8*}* 
  %63 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %62, i32 0, i32 0 
  %64 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %63, align 8 
  %65 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %62, i32 0, i32 1 
  %66 = load  i8*, i8** %65, align 8 
  %67 =  call ccc  i8*  %64(i8*  %66, i8*  %61)  
  %68 = bitcast i8* %67 to {i8*, i8*}* 
  %69 = bitcast {i8*, i8*}* %68 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %70 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %7)  
  %71 = and i1 %70, 1 
  br i1 %71, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %72 = bitcast {i8*, i8*}* %6 to i8* 
  %73 = bitcast {i8* (i8*, i8*)*, {}*}* @reverse to {i8* (i8*, i8*)*, i8*}* 
  %74 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %73, i32 0, i32 0 
  %75 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %74, align 8 
  %76 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %73, i32 0, i32 1 
  %77 = load  i8*, i8** %76, align 8 
  %78 =  call ccc  i8*  %75(i8*  %77, i8*  %72)  
  %79 = bitcast i8* %78 to {i8*, i8*}* 
  %80 = bitcast {i8*, i8*}* %79 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %81 = phi i8* [%69, %branchExpBlock_0], [%80, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %82 = bitcast i8* %81 to {i8*, i8*}* 
  %83 = bitcast {i8*, i8*}* %82 to i8* 
  ret i8* %83 
}


define external ccc  i8* @$helper$closureFn$7(i8*  %env_0, i8*  %acc_0)    {
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
  store  i8* (i8*, i8*)* @$closureFn$8, i8* (i8*, i8*)** %12, align 8 
  %13 = getelementptr  {i8* (i8*, i8*)*, {i8*, i8*}*}, {i8* (i8*, i8*)*, {i8*, i8*}*}* %11, i32 0, i32 1 
  store  {i8*, i8*}* %7, {i8*, i8*}** %13, align 8 
  %14 = bitcast {i8* (i8*, i8*)*, {i8*, i8*}*}* %11 to i8* 
  ret i8* %14 
}


define external ccc  i8* @$closureFn$9(i8*  %env_0, i8*  %m_0)    {
  %1 = bitcast i8* %env_0 to {i8*}* 
  %2 = getelementptr  {i8*}, {i8*}* %1, i32 0, i32 0 
  %3 = load  i8*, i8** %2, align 8 
  %4 = bitcast i8* %m_0 to {i8*, i8*}* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8*}* 
  %7 = getelementptr  {i8*}, {i8*}* %6, i32 0, i32 0 
  store  i8* %3, i8** %7, align 8 
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %9 = bitcast i8* %8 to {i8* (i8*, i8*)*, {i8*}*}* 
  %10 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %9, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$helper$closureFn$7, i8* (i8*, i8*)** %10, align 8 
  %11 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %9, i32 0, i32 1 
  store  {i8*}* %6, {i8*}** %11, align 8 
  %12 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %13 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %9 to {i8* (i8*, i8*)*, i8*}* 
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


declare external ccc  double @MadList_length({i8*, i8*}*)    


define external ccc  i8* @len$0(i8*  %env_0, i8*  %arg_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %arg_0 to {i8*, i8*}* 
  %3 =  call ccc  double  @MadList_length({i8*, i8*}*  %2)  
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  ret i8* %6 
}


@len =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @len$0, {}* zeroinitializer }


declare external ccc  {i8*, i8*}* @MadList_push(i8*, {i8*, i8*}*)    


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
  %6 =  call ccc  {i8*, i8*}*  @MadList_push(i8*  %4, {i8*, i8*}*  %5)  
  %7 = bitcast {i8*, i8*}* %6 to i8* 
  ret i8* %7 
}


@push =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @push$0, {}* zeroinitializer }


@fn =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @fn$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @fn$fn(i8*  %env_0, i8*  %__0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %__0 to i8* 
  %3 = load  {i8*, i8*}*, {i8*, i8*}** @l1, align 8 
  %4 = bitcast {i8*, i8*}* %3 to i8* 
  ret i8* %4 
}


@reverse =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @reverse$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @reverse$fn(i8*  %env_0, i8*  %xs_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %xs_0 to {i8*, i8*}* 
  %3 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %4 = bitcast i8* %3 to {}* 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %6 = bitcast i8* %5 to {i8* (i8*, i8*)*, {}*}* 
  %7 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %6, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$helper$closureFn$0, i8* (i8*, i8*)** %7, align 8 
  %8 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %6, i32 0, i32 1 
  store  {}* %4, {}** %8, align 8 
  %9 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %10 = bitcast {i8* (i8*, i8*)*, {}*}* %6 to {i8* (i8*, i8*)*, i8*}* 
  %11 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 0 
  %12 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %11, align 8 
  %13 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %10, i32 0, i32 1 
  %14 = load  i8*, i8** %13, align 8 
  %15 =  call ccc  i8*  %12(i8*  %14, i8*  %9)  
  %16 = bitcast i8* %15 to {i8*, i8*}* 
  %17 = bitcast {i8*, i8*}* %2 to i8* 
  %18 = bitcast {i8*, i8*}* %16 to {i8* (i8*, i8*)*, i8*}* 
  %19 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %18, i32 0, i32 0 
  %20 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %19, align 8 
  %21 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %18, i32 0, i32 1 
  %22 = load  i8*, i8** %21, align 8 
  %23 =  call ccc  i8*  %20(i8*  %22, i8*  %17)  
  %24 = bitcast i8* %23 to {i8*, i8*}* 
  %25 = bitcast {i8*, i8*}* %24 to i8* 
  ret i8* %25 
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
  store  i8* (i8*, i8*)* @$closureFn$3, i8* (i8*, i8*)** %16, align 8 
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


define external ccc  i8* @hop$fn(i8*  %env_0, i8*  %x_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %x_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 = fsub double %3, 1.000000e0 
  %5 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %6 = bitcast i8* %5 to double* 
  store  double %4, double* %6, align 8 
  %7 = bitcast double* %6 to i8* 
  ret i8* %7 
}


@ch =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @ch$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @ch$fn(i8*  %env_0, i8*  %count_0)    {
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %count_0 to double* 
  %3 = load  double, double* %2, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %5 = bitcast i8* %4 to double* 
  store  double %3, double* %5, align 8 
  %6 = bitcast double* %5 to i8* 
  %7 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*}* getelementptr inbounds ({i8*}, {i8*}* inttoptr (i32 0 to {i8*}*), i32 1) to i64))  
  %8 = bitcast i8* %7 to {i8*}* 
  %9 = getelementptr  {i8*}, {i8*}* %8, i32 0, i32 0 
  store  i8* %6, i8** %9, align 8 
  %10 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {i8*}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {i8*}*}*), i32 1) to i64))  
  %11 = bitcast i8* %10 to {i8* (i8*, i8*)*, {i8*}*}* 
  %12 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %11, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$5, i8* (i8*, i8*)** %12, align 8 
  %13 = getelementptr  {i8* (i8*, i8*)*, {i8*}*}, {i8* (i8*, i8*)*, {i8*}*}* %11, i32 0, i32 1 
  store  {i8*}* %8, {i8*}** %13, align 8 
  %14 = bitcast {i8* (i8*, i8*)*, {i8*}*}* %11 to i8* 
  ret i8* %14 
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
  %7 = bitcast {i8* (i8*, i8*)*, {}*}* @ch to {i8* (i8*, i8*)*, i8*}* 
  %8 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %7, i32 0, i32 0 
  %9 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %8, align 8 
  %10 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %7, i32 0, i32 1 
  %11 = load  i8*, i8** %10, align 8 
  %12 =  call ccc  i8*  %9(i8*  %11, i8*  %6)  
  %13 = bitcast i8* %12 to {i8*, i8*}* 
  %14 = bitcast {i8*, i8*}* zeroinitializer to i8* 
  %15 = bitcast {i8*, i8*}* %13 to {i8* (i8*, i8*)*, i8*}* 
  %16 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %15, i32 0, i32 0 
  %17 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %16, align 8 
  %18 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %15, i32 0, i32 1 
  %19 = load  i8*, i8** %18, align 8 
  %20 =  call ccc  i8*  %17(i8*  %19, i8*  %14)  
  %21 = bitcast i8* %20 to {i8*, i8*}* 
  %22 = bitcast {i8*, i8*}* %21 to i8* 
  ret i8* %22 
}


declare external ccc  {i8*, i8*}* @MadList_map(i8*, {i8*, i8*}*)    


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
  %5 =  call ccc  {i8*, i8*}*  @MadList_map(i8*  %3, {i8*, i8*}*  %4)  
  %6 = bitcast {i8*, i8*}* %5 to i8* 
  ret i8* %6 
}


@map2 =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @map2$0, {}* zeroinitializer }


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


@l1 =    global {i8*, i8*}* undef


@l2 =    global {i8*, i8*}* undef


@l3 =    global {i8*, i8*}* undef


@mapped =    global {i8*, i8*}* undef


@r =    global i8* undef


@top =    global double undef


@e =    global i8* undef


@big =    global {i8*, i8*}* undef


@result =    global {i8*, i8*}* undef


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
  %7 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %6, {i8*, i8*}*  %3)  
  %8 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %9 = bitcast i8* %8 to double* 
  store  double 1.000000e0, double* %9, align 8 
  %10 = bitcast double* %9 to i8* 
  %11 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %10, {i8*, i8*}*  %7)  
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
  %19 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %18, {i8*, i8*}*  %15)  
  %20 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %21 = bitcast i8* %20 to double* 
  store  double 4.000000e0, double* %21, align 8 
  %22 = bitcast double* %21 to i8* 
  %23 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %22, {i8*, i8*}*  %19)  
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
  %35 =  call ccc  i1  @MadList_hasMinLength(double  2.000000e0, {i8*, i8*}*  %34)  
  %36 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 0 
  %37 = load  i8*, i8** %36, align 8 
  %38 = bitcast i8* %37 to {i8*, i8*}* 
  %39 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 1 
  %40 = load  i8*, i8** %39, align 8 
  %41 = bitcast i8* %40 to {i8*, i8*}* 
  %42 = getelementptr  {i8*, i8*}, {i8*, i8*}* %41, i32 0, i32 0 
  %43 = load  i8*, i8** %42, align 8 
  %44 = bitcast i8* %43 to {i8*, i8*}* 
  %45 = getelementptr  {i8*, i8*}, {i8*, i8*}* %41, i32 0, i32 1 
  %46 = load  i8*, i8** %45, align 8 
  %47 = bitcast i8* %46 to {i8*, i8*}* 
  %48 = and i1 1, 1 
  %49 = and i1 1, %48 
  %50 = and i1 %35, %49 
  br i1 %50, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %51 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 0 
  %52 = load  i8*, i8** %51, align 8 
  %53 = bitcast i8* %52 to {i8*, i8*}* 
  %54 = getelementptr  {i8*, i8*}, {i8*, i8*}* %34, i32 0, i32 1 
  %55 = load  i8*, i8** %54, align 8 
  %56 = bitcast i8* %55 to {i8*, i8*}* 
  %57 = getelementptr  {i8*, i8*}, {i8*, i8*}* %56, i32 0, i32 0 
  %58 = load  i8*, i8** %57, align 8 
  %59 = bitcast i8* %58 to {i8*, i8*}* 
  %60 = getelementptr  {i8*, i8*}, {i8*, i8*}* %56, i32 0, i32 1 
  %61 = load  i8*, i8** %60, align 8 
  %62 = bitcast i8* %61 to {i8*, i8*}* 
  %63 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %64 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %63, align 8 
  %65 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %64()  
  %66 = bitcast {i8*, i8*}* %59 to i8* 
  %67 = bitcast {i8* (i8*, i8*)*, {}*}* %65 to {i8* (i8*, i8*)*, i8*}* 
  %68 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %67, i32 0, i32 0 
  %69 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %68, align 8 
  %70 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %67, i32 0, i32 1 
  %71 = load  i8*, i8** %70, align 8 
  %72 =  call ccc  i8*  %69(i8*  %71, i8*  %66)  
  %73 = bitcast i8* %72 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %74 = phi i8* [%73, %branchExpBlock_0], [zeroinitializer, %entry_0] 
  %75 = bitcast i8* %74 to i8* 
  %76 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %77 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %76, i32 0, i32 0 
  %78 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %77, align 8 
  %79 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %76, i32 0, i32 1 
  %80 = load  i8*, i8** %79, align 8 
  %81 =  call ccc  i8*  %78(i8*  %80, i8*  %75)  
  %82 = bitcast i8* %81 to i8* 
  %83 = getelementptr  {{i8* (i8*, i8*)*, {}*}*}, {{i8* (i8*, i8*)*, {}*}*}* @$Functor$List, i32 0, i32 0 
  %84 = load  {i8* (i8*, i8*)*, {}*}*, {i8* (i8*, i8*)*, {}*}** %83, align 8 
  %85 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %86 = bitcast i8* %85 to {}* 
  %87 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %88 = bitcast i8* %87 to {i8* (i8*, i8*)*, {}*}* 
  %89 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %88, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$closureFn$2, i8* (i8*, i8*)** %89, align 8 
  %90 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %88, i32 0, i32 1 
  store  {}* %86, {}** %90, align 8 
  %91 = bitcast {i8* (i8*, i8*)*, {}*}* %88 to i8* 
  %92 = bitcast {i8* (i8*, i8*)*, {}*}* %84 to {i8* (i8*, i8*)*, i8*}* 
  %93 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %92, i32 0, i32 0 
  %94 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %93, align 8 
  %95 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %92, i32 0, i32 1 
  %96 = load  i8*, i8** %95, align 8 
  %97 =  call ccc  i8*  %94(i8*  %96, i8*  %91)  
  %98 = bitcast i8* %97 to i8* 
  %99 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %100 = bitcast i8* %99 to double* 
  store  double 3.000000e0, double* %100, align 8 
  %101 = bitcast double* %100 to i8* 
  %102 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %101)  
  %103 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %104 = bitcast i8* %103 to double* 
  store  double 2.000000e0, double* %104, align 8 
  %105 = bitcast double* %104 to i8* 
  %106 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %105, {i8*, i8*}*  %102)  
  %107 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %108 = bitcast i8* %107 to double* 
  store  double 1.000000e0, double* %108, align 8 
  %109 = bitcast double* %108 to i8* 
  %110 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %109, {i8*, i8*}*  %106)  
  %111 = bitcast {i8*, i8*}* %110 to i8* 
  %112 = bitcast i8* %98 to {i8* (i8*, i8*)*, i8*}* 
  %113 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %112, i32 0, i32 0 
  %114 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %113, align 8 
  %115 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %112, i32 0, i32 1 
  %116 = load  i8*, i8** %115, align 8 
  %117 =  call ccc  i8*  %114(i8*  %116, i8*  %111)  
  %118 = bitcast i8* %117 to {i8*, i8*}* 
  store  {i8*, i8*}* %118, {i8*, i8*}** @mapped, align 8 
  %119 =  call ccc  i1  @MadList_hasMinLength(double  3.000000e0, {i8*, i8*}*  %118)  
  %120 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 0 
  %121 = load  i8*, i8** %120, align 8 
  %122 = bitcast i8* %121 to {i8*, i8*}* 
  %123 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 1 
  %124 = load  i8*, i8** %123, align 8 
  %125 = bitcast i8* %124 to {i8*, i8*}* 
  %126 = getelementptr  {i8*, i8*}, {i8*, i8*}* %125, i32 0, i32 0 
  %127 = load  i8*, i8** %126, align 8 
  %128 = bitcast i8* %127 to {i8*, i8*}* 
  %129 = getelementptr  {i8*, i8*}, {i8*, i8*}* %125, i32 0, i32 1 
  %130 = load  i8*, i8** %129, align 8 
  %131 = bitcast i8* %130 to {i8*, i8*}* 
  %132 = getelementptr  {i8*, i8*}, {i8*, i8*}* %131, i32 0, i32 0 
  %133 = load  i8*, i8** %132, align 8 
  %134 = bitcast i8* %133 to {i8*, i8*}* 
  %135 = getelementptr  {i8*, i8*}, {i8*, i8*}* %131, i32 0, i32 1 
  %136 = load  i8*, i8** %135, align 8 
  %137 = bitcast i8* %136 to {i8*, i8*}* 
  %138 = and i1 1, 1 
  %139 = and i1 1, %138 
  %140 = and i1 1, %139 
  %141 = and i1 %119, %140 
  br i1 %141, label %branchExpBlock_1, label %nextBlock_0 
branchExpBlock_1:
  %142 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 0 
  %143 = load  i8*, i8** %142, align 8 
  %144 = bitcast i8* %143 to {i8*, i8*}* 
  %145 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 1 
  %146 = load  i8*, i8** %145, align 8 
  %147 = bitcast i8* %146 to {i8*, i8*}* 
  %148 = getelementptr  {i8*, i8*}, {i8*, i8*}* %147, i32 0, i32 0 
  %149 = load  i8*, i8** %148, align 8 
  %150 = bitcast i8* %149 to {i8*, i8*}* 
  %151 = getelementptr  {i8*, i8*}, {i8*, i8*}* %147, i32 0, i32 1 
  %152 = load  i8*, i8** %151, align 8 
  %153 = bitcast i8* %152 to {i8*, i8*}* 
  %154 = getelementptr  {i8*, i8*}, {i8*, i8*}* %153, i32 0, i32 0 
  %155 = load  i8*, i8** %154, align 8 
  %156 = bitcast i8* %155 to {i8*, i8*}* 
  %157 = getelementptr  {i8*, i8*}, {i8*, i8*}* %153, i32 0, i32 1 
  %158 = load  i8*, i8** %157, align 8 
  %159 = bitcast i8* %158 to {i8*, i8*}* 
  %160 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %161 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %160, align 8 
  %162 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %161()  
  %163 = bitcast {i8*, i8*}* %156 to i8* 
  %164 = bitcast {i8* (i8*, i8*)*, {}*}* %162 to {i8* (i8*, i8*)*, i8*}* 
  %165 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %164, i32 0, i32 0 
  %166 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %165, align 8 
  %167 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %164, i32 0, i32 1 
  %168 = load  i8*, i8** %167, align 8 
  %169 =  call ccc  i8*  %166(i8*  %168, i8*  %163)  
  %170 = bitcast i8* %169 to i8* 
  br label %exitBlock_1 
nextBlock_0:
  %171 =  call ccc  i1  @MadList_hasMinLength(double  1.000000e0, {i8*, i8*}*  %118)  
  %172 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 0 
  %173 = load  i8*, i8** %172, align 8 
  %174 = bitcast i8* %173 to {i8*, i8*}* 
  %175 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 1 
  %176 = load  i8*, i8** %175, align 8 
  %177 = bitcast i8* %176 to {i8*, i8*}* 
  %178 = and i1 1, 1 
  %179 = and i1 %171, %178 
  br i1 %179, label %branchExpBlock_2, label %nextBlock_1 
branchExpBlock_2:
  %180 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 0 
  %181 = load  i8*, i8** %180, align 8 
  %182 = bitcast i8* %181 to {i8*, i8*}* 
  %183 = getelementptr  {i8*, i8*}, {i8*, i8*}* %118, i32 0, i32 1 
  %184 = load  i8*, i8** %183, align 8 
  %185 = bitcast i8* %184 to {i8*, i8*}* 
  %186 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %187 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %186, align 8 
  %188 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %187()  
  %189 = bitcast {i8*, i8*}* %182 to i8* 
  %190 = bitcast {i8* (i8*, i8*)*, {}*}* %188 to {i8* (i8*, i8*)*, i8*}* 
  %191 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %190, i32 0, i32 0 
  %192 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %191, align 8 
  %193 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %190, i32 0, i32 1 
  %194 = load  i8*, i8** %193, align 8 
  %195 =  call ccc  i8*  %192(i8*  %194, i8*  %189)  
  %196 = bitcast i8* %195 to i8* 
  br label %exitBlock_1 
nextBlock_1:
  %197 =  call ccc  i1  @MadList_hasLength(double  0.000000e0, {i8*, i8*}*  %118)  
  %198 = and i1 %197, 1 
  br i1 %198, label %branchExpBlock_3, label %exitBlock_1 
branchExpBlock_3:
  %199 =  call ccc  i8*  @GC_malloc(i64  6)  
  %200 = getelementptr  i8, i8* %199, i32 0 
  store  i8 101, i8* %200, align 8 
  %201 = getelementptr  i8, i8* %199, i32 1 
  store  i8 109, i8* %201, align 8 
  %202 = getelementptr  i8, i8* %199, i32 2 
  store  i8 112, i8* %202, align 8 
  %203 = getelementptr  i8, i8* %199, i32 3 
  store  i8 116, i8* %203, align 8 
  %204 = getelementptr  i8, i8* %199, i32 4 
  store  i8 121, i8* %204, align 8 
  %205 = getelementptr  i8, i8* %199, i32 5 
  store  i8 0, i8* %205, align 8 
  br label %exitBlock_1 
exitBlock_1:
  %206 = phi i8* [%170, %branchExpBlock_1], [%196, %branchExpBlock_2], [%199, %branchExpBlock_3], [zeroinitializer, %nextBlock_1] 
  %207 = bitcast i8* %206 to i8* 
  store  i8* %207, i8** @r, align 8 
  %208 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %209 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %208, i32 0, i32 0 
  %210 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %209, align 8 
  %211 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %208, i32 0, i32 1 
  %212 = load  i8*, i8** %211, align 8 
  %213 =  call ccc  i8*  %210(i8*  %212, i8*  %207)  
  %214 = bitcast i8* %213 to i8* 
  %215 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %216 = bitcast i8* %215 to double* 
  store  double 3.000000e0, double* %216, align 8 
  %217 = bitcast double* %216 to i8* 
  %218 = bitcast {i8* (i8*, i8*)*, {}*}* @nth to {i8* (i8*, i8*)*, i8*}* 
  %219 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %218, i32 0, i32 0 
  %220 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %219, align 8 
  %221 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %218, i32 0, i32 1 
  %222 = load  i8*, i8** %221, align 8 
  %223 =  call ccc  i8*  %220(i8*  %222, i8*  %217)  
  %224 = bitcast i8* %223 to i8* 
  %225 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %226 = bitcast i8* %225 to double* 
  store  double 5.000000e0, double* %226, align 8 
  %227 = bitcast double* %226 to i8* 
  %228 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %227)  
  %229 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %230 = bitcast i8* %229 to double* 
  store  double 4.000000e0, double* %230, align 8 
  %231 = bitcast double* %230 to i8* 
  %232 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %231, {i8*, i8*}*  %228)  
  %233 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %234 = bitcast i8* %233 to double* 
  store  double 3.000000e0, double* %234, align 8 
  %235 = bitcast double* %234 to i8* 
  %236 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %235, {i8*, i8*}*  %232)  
  %237 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %238 = bitcast i8* %237 to double* 
  store  double 2.000000e0, double* %238, align 8 
  %239 = bitcast double* %238 to i8* 
  %240 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %239, {i8*, i8*}*  %236)  
  %241 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %242 = bitcast i8* %241 to double* 
  store  double 1.000000e0, double* %242, align 8 
  %243 = bitcast double* %242 to i8* 
  %244 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %243, {i8*, i8*}*  %240)  
  %245 = bitcast {i8*, i8*}* %244 to i8* 
  %246 = bitcast i8* %224 to {i8* (i8*, i8*)*, i8*}* 
  %247 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %246, i32 0, i32 0 
  %248 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %247, align 8 
  %249 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %246, i32 0, i32 1 
  %250 = load  i8*, i8** %249, align 8 
  %251 =  call ccc  i8*  %248(i8*  %250, i8*  %245)  
  %252 = bitcast i8* %251 to i8* 
  %253 = bitcast i8* %252 to {i64, i8*}* 
  %254 = getelementptr  {i64, i8*}, {i64, i8*}* %253, i32 0, i32 1 
  %255 = getelementptr  {i64, i8*}, {i64, i8*}* %253, i32 0, i32 0 
  %256 = load  i64, i64* %255, align 8 
  %257 = icmp eq i64 0, %256 
  %258 = load  i8*, i8** %254, align 8 
  %259 = bitcast i8* %258 to i8* 
  %260 = and i1 1, 1 
  %261 = and i1 %257, %260 
  br i1 %261, label %branchExpBlock_4, label %nextBlock_2 
branchExpBlock_4:
  %262 = bitcast i8* %252 to {i64, i8*}* 
  %263 = getelementptr  {i64, i8*}, {i64, i8*}* %262, i32 0, i32 1 
  %264 = load  i8*, i8** %263, align 8 
  %265 = bitcast i8* %264 to i8* 
  %266 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %267 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %266, align 8 
  %268 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %267()  
  %269 = bitcast {i8* (i8*, i8*)*, {}*}* %268 to {i8* (i8*, i8*)*, i8*}* 
  %270 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %269, i32 0, i32 0 
  %271 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %270, align 8 
  %272 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %269, i32 0, i32 1 
  %273 = load  i8*, i8** %272, align 8 
  %274 =  call ccc  i8*  %271(i8*  %273, i8*  %265)  
  %275 = bitcast i8* %274 to i8* 
  %276 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %277 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %276, i32 0, i32 0 
  %278 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %277, align 8 
  %279 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %276, i32 0, i32 1 
  %280 = load  i8*, i8** %279, align 8 
  %281 =  call ccc  i8*  %278(i8*  %280, i8*  %275)  
  %282 = bitcast i8* %281 to i8* 
  br label %exitBlock_2 
nextBlock_2:
  %283 = bitcast i8* %252 to {i64}* 
  %284 = getelementptr  {i64}, {i64}* %283, i32 0, i32 0 
  %285 = load  i64, i64* %284, align 8 
  %286 = icmp eq i64 1, %285 
  %287 = and i1 %286, 1 
  br i1 %287, label %branchExpBlock_5, label %exitBlock_2 
branchExpBlock_5:
  %288 = bitcast i8* %252 to {i64}* 
  %289 =  call ccc  i8*  @GC_malloc(i64  8)  
  %290 = getelementptr  i8, i8* %289, i32 0 
  store  i8 110, i8* %290, align 8 
  %291 = getelementptr  i8, i8* %289, i32 1 
  store  i8 111, i8* %291, align 8 
  %292 = getelementptr  i8, i8* %289, i32 2 
  store  i8 116, i8* %292, align 8 
  %293 = getelementptr  i8, i8* %289, i32 3 
  store  i8 104, i8* %293, align 8 
  %294 = getelementptr  i8, i8* %289, i32 4 
  store  i8 105, i8* %294, align 8 
  %295 = getelementptr  i8, i8* %289, i32 5 
  store  i8 110, i8* %295, align 8 
  %296 = getelementptr  i8, i8* %289, i32 6 
  store  i8 103, i8* %296, align 8 
  %297 = getelementptr  i8, i8* %289, i32 7 
  store  i8 0, i8* %297, align 8 
  %298 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %299 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %298, i32 0, i32 0 
  %300 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %299, align 8 
  %301 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %298, i32 0, i32 1 
  %302 = load  i8*, i8** %301, align 8 
  %303 =  call ccc  i8*  %300(i8*  %302, i8*  %289)  
  %304 = bitcast i8* %303 to i8* 
  br label %exitBlock_2 
exitBlock_2:
  %305 = phi i8* [%282, %branchExpBlock_4], [%304, %branchExpBlock_5], [zeroinitializer, %nextBlock_2] 
  %306 = bitcast i8* %305 to i8* 
  store  double 1.900000e1, double* @top, align 8 
  %307 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %308 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %307, align 8 
  %309 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %308()  
  %310 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %311 = bitcast i8* %310 to double* 
  store  double 3.000000e0, double* %311, align 8 
  %312 = bitcast double* %311 to i8* 
  %313 = bitcast {i8* (i8*, i8*)*, {}*}* @getTop to {i8* (i8*, i8*)*, i8*}* 
  %314 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %313, i32 0, i32 0 
  %315 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %314, align 8 
  %316 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %313, i32 0, i32 1 
  %317 = load  i8*, i8** %316, align 8 
  %318 =  call ccc  i8*  %315(i8*  %317, i8*  %312)  
  %319 = bitcast i8* %318 to double* 
  %320 = load  double, double* %319, align 8 
  %321 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %322 = bitcast i8* %321 to double* 
  store  double %320, double* %322, align 8 
  %323 = bitcast double* %322 to i8* 
  %324 = bitcast {i8* (i8*, i8*)*, {}*}* %309 to {i8* (i8*, i8*)*, i8*}* 
  %325 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %324, i32 0, i32 0 
  %326 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %325, align 8 
  %327 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %324, i32 0, i32 1 
  %328 = load  i8*, i8** %327, align 8 
  %329 =  call ccc  i8*  %326(i8*  %328, i8*  %323)  
  %330 = bitcast i8* %329 to i8* 
  %331 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %332 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %331, i32 0, i32 0 
  %333 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %332, align 8 
  %334 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %331, i32 0, i32 1 
  %335 = load  i8*, i8** %334, align 8 
  %336 =  call ccc  i8*  %333(i8*  %335, i8*  %330)  
  %337 = bitcast i8* %336 to i8* 
  %338 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  %339 = bitcast i8* %338 to {}* 
  %340 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  %341 = bitcast i8* %340 to {i8* (i8*, i8*)*, {}*}* 
  %342 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %341, i32 0, i32 0 
  store  i8* (i8*, i8*)* @$inc$closureFn$4, i8* (i8*, i8*)** %342, align 8 
  %343 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %341, i32 0, i32 1 
  store  {}* %339, {}** %343, align 8 
  %344 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %345 = bitcast i8* %344 to double* 
  store  double 9.000000e0, double* %345, align 8 
  %346 = bitcast double* %345 to i8* 
  %347 = bitcast {i8* (i8*, i8*)*, {}*}* %341 to {i8* (i8*, i8*)*, i8*}* 
  %348 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %347, i32 0, i32 0 
  %349 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %348, align 8 
  %350 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %347, i32 0, i32 1 
  %351 = load  i8*, i8** %350, align 8 
  %352 =  call ccc  i8*  %349(i8*  %351, i8*  %346)  
  %353 = bitcast i8* %352 to double* 
  %354 = load  double, double* %353, align 8 
  %355 = fmul double 4.000000e0, %354 
  %356 = fadd double 2.000000e0, %355 
  %357 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  %358 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %357, align 8 
  %359 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %358()  
  %360 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %361 = bitcast i8* %360 to double* 
  store  double %356, double* %361, align 8 
  %362 = bitcast double* %361 to i8* 
  %363 = bitcast {i8* (i8*, i8*)*, {}*}* %359 to {i8* (i8*, i8*)*, i8*}* 
  %364 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %363, i32 0, i32 0 
  %365 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %364, align 8 
  %366 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %363, i32 0, i32 1 
  %367 = load  i8*, i8** %366, align 8 
  %368 =  call ccc  i8*  %365(i8*  %367, i8*  %362)  
  %369 = bitcast i8* %368 to i8* 
  store  i8* %369, i8** @e, align 8 
  %370 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  %371 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %370, i32 0, i32 0 
  %372 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %371, align 8 
  %373 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %370, i32 0, i32 1 
  %374 = load  i8*, i8** %373, align 8 
  %375 =  call ccc  i8*  %372(i8*  %374, i8*  %369)  
  %376 = bitcast i8* %375 to i8* 
  %377 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %378 = bitcast i8* %377 to double* 
  store  double 1.000000e6, double* %378, align 8 
  %379 = bitcast double* %378 to i8* 
  %380 = bitcast {i8* (i8*, i8*)*, {}*}* @createBigList to {i8* (i8*, i8*)*, i8*}* 
  %381 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %380, i32 0, i32 0 
  %382 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %381, align 8 
  %383 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %380, i32 0, i32 1 
  %384 = load  i8*, i8** %383, align 8 
  ; %385 =  call ccc  i8*  %382(i8*  %384, i8*  %379)  
  ; %386 = bitcast i8* %385 to {i8*, i8*}* 
  ; store  {i8*, i8*}* %386, {i8*, i8*}** @big, align 8 
  ; %387 =  call ccc  i8*  @GC_malloc(i64  8)  
  ; %388 = getelementptr  i8, i8* %387, i32 0 
  ; store  i8 99, i8* %388, align 8 
  ; %389 = getelementptr  i8, i8* %387, i32 1 
  ; store  i8 114, i8* %389, align 8 
  ; %390 = getelementptr  i8, i8* %387, i32 2 
  ; store  i8 101, i8* %390, align 8 
  ; %391 = getelementptr  i8, i8* %387, i32 3 
  ; store  i8 97, i8* %391, align 8 
  ; %392 = getelementptr  i8, i8* %387, i32 4 
  ; store  i8 116, i8* %392, align 8 
  ; %393 = getelementptr  i8, i8* %387, i32 5 
  ; store  i8 101, i8* %393, align 8 
  ; %394 = getelementptr  i8, i8* %387, i32 6 
  ; store  i8 100, i8* %394, align 8 
  ; %395 = getelementptr  i8, i8* %387, i32 7 
  ; store  i8 0, i8* %395, align 8 
  ; %396 = bitcast {i8* (i8*, i8*)*, {}*}* @log to {i8* (i8*, i8*)*, i8*}* 
  ; %397 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %396, i32 0, i32 0 
  ; %398 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %397, align 8 
  ; %399 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %396, i32 0, i32 1 
  ; %400 = load  i8*, i8** %399, align 8 
  ; %401 =  call ccc  i8*  %398(i8*  %400, i8*  %387)  
  ; %402 = bitcast i8* %401 to i8* 
  ; %403 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({}* getelementptr inbounds ({}, {}* inttoptr (i32 0 to {}*), i32 1) to i64))  
  ; %404 = bitcast i8* %403 to {}* 
  ; %405 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8* (i8*, i8*)*, {}*}* getelementptr inbounds ({i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* inttoptr (i32 0 to {i8* (i8*, i8*)*, {}*}*), i32 1) to i64))  
  ; %406 = bitcast i8* %405 to {i8* (i8*, i8*)*, {}*}* 
  ; %407 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %406, i32 0, i32 0 
  ; store  i8* (i8*, i8*)* @$closureFn$6, i8* (i8*, i8*)** %407, align 8 
  ; %408 = getelementptr  {i8* (i8*, i8*)*, {}*}, {i8* (i8*, i8*)*, {}*}* %406, i32 0, i32 1 
  ; store  {}* %404, {}** %408, align 8 
  ; %409 = bitcast {i8* (i8*, i8*)*, {}*}* %406 to i8* 
  ; %410 = bitcast {i8* (i8*, i8*)*, {}*}* @map2 to {i8* (i8*, i8*)*, i8*}* 
  ; %411 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %410, i32 0, i32 0 
  ; %412 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %411, align 8 
  ; %413 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %410, i32 0, i32 1 
  ; %414 = load  i8*, i8** %413, align 8 
  ; %415 =  call ccc  i8*  %412(i8*  %414, i8*  %409)  
  ; %416 = bitcast i8* %415 to {i8*, i8*}* 
  ; %417 = bitcast {i8*, i8*}* %386 to i8* 
  ; %418 = bitcast {i8*, i8*}* %416 to {i8* (i8*, i8*)*, i8*}* 
  ; %419 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %418, i32 0, i32 0 
  ; %420 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %419, align 8 
  ; %421 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %418, i32 0, i32 1 
  ; %422 = load  i8*, i8** %421, align 8 
  ; %423 =  call ccc  i8*  %420(i8*  %422, i8*  %417)  
  ; %424 = bitcast i8* %423 to {i8*, i8*}* 
  ; store  {i8*, i8*}* %424, {i8*, i8*}** @result, align 8 
  ; %425 = getelementptr  {{i8* (i8*, i8*)*, {}*}* ()*}, {{i8* (i8*, i8*)*, {}*}* ()*}* @$Show$Number, i32 0, i32 0 
  ; %426 = load  {i8* (i8*, i8*)*, {}*}* ()*, {i8* (i8*, i8*)*, {}*}* ()** %425, align 8 
  ; %427 =  call ccc  {i8* (i8*, i8*)*, {}*}*  %426()  
  ; %428 = bitcast {i8*, i8*}* %424 to i8* 
  ; %429 = bitcast {i8* (i8*, i8*)*, {}*}* @len to {i8* (i8*, i8*)*, i8*}* 
  ; %430 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %429, i32 0, i32 0 
  ; %431 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %430, align 8 
  ; %432 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %429, i32 0, i32 1 
  ; %433 = load  i8*, i8** %432, align 8 
  ; %434 =  call ccc  i8*  %431(i8*  %433, i8*  %428)  
  ret void 
}