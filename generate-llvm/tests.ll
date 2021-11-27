; ModuleID = 'main'


 


declare external ccc  i8* @__e9f54377746cb0755b8ab19b5a3e8824__Just(i8*)    


declare external ccc  i8* @__e9f54377746cb0755b8ab19b5a3e8824__Nothing()    


@__4282899fd67a43a38f13e2be62dba7a9__NUM_EXPORTED = external   global double* 


@$Applicative$List = external   global {i8* (i8*, i8*)*, i8* (i8*)*}* 


declare external ccc  i8* @$Applicative$List$ap(i8*, i8*)    


declare external ccc  i8* @$Applicative$List$pure(i8*)    


@$Functor$List = external   global {i8* (i8*, i8*)*}* 


declare external ccc  i8* @$Functor$List$map(i8*, i8*)    


@$Functor$Maybe = external   global {i8* (i8*, i8*)*}* 


declare external ccc  i8* @$Functor$Maybe$map(i8*, i8*)    


declare external ccc  i8* @puts(i8*)    


define external ccc  i8* @__234a9bccfad05393b28dc26cfaded70a__log(i8* )    {
  %2 =  call ccc  i8*  @puts(i8*  %0)  
  ret i8* %2 
}


declare external ccc  i8* @__doubleToStr__(i8*)    


define external ccc  i8* @__234a9bccfad05393b28dc26cfaded70a__showNumber(i8* )    {
  %2 =  call ccc  i8*  @__doubleToStr__(i8*  %0)  
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
  %0 =  call ccc  i8*  @GC_malloc(i64  3)  
  %1 = getelementptr  i8, i8* %0, i32 0 
  store  i8 111, i8* %1, align 8 
  %2 = getelementptr  i8, i8* %0, i32 1 
  store  i8 107, i8* %2, align 8 
  %3 = getelementptr  i8, i8* %0, i32 2 
  store  i8 0, i8* %3, align 8 
  %4 =  call ccc  i8*  @__e9f54377746cb0755b8ab19b5a3e8824__Just(i8*  %0)  
  %5 = bitcast i8* %4 to i8* 
  %6 = bitcast i8* %5 to {i64}* 
  %7 = getelementptr  {i64}, {i64}* %6, i32 0, i32 0 
  %8 = load  i64, i64* %7, align 8 
  %9 = icmp eq i64 1, %8 
  %10 = and i1 %9, 1 
  br i1 %10, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %11 = bitcast i8* %5 to {i64}* 
  %12 =  call ccc  i8*  @GC_malloc(i64  5)  
  %13 = getelementptr  i8, i8* %12, i32 0 
  store  i8 110, i8* %13, align 8 
  %14 = getelementptr  i8, i8* %12, i32 1 
  store  i8 111, i8* %14, align 8 
  %15 = getelementptr  i8, i8* %12, i32 2 
  store  i8 112, i8* %15, align 8 
  %16 = getelementptr  i8, i8* %12, i32 3 
  store  i8 101, i8* %16, align 8 
  %17 = getelementptr  i8, i8* %12, i32 4 
  store  i8 0, i8* %17, align 8 
  %18 =  call ccc  i8*  @__234a9bccfad05393b28dc26cfaded70a__log(i8*  %12)  
  %19 = bitcast i8* %18 to i8* 
  br label %exitBlock_0 
nextBlock_0:
  %20 = bitcast i8* %5 to {i64, i8*}* 
  %21 = getelementptr  {i64, i8*}, {i64, i8*}* %20, i32 0, i32 1 
  %22 = getelementptr  {i64, i8*}, {i64, i8*}* %20, i32 0, i32 0 
  %23 = load  i64, i64* %22, align 8 
  %24 = icmp eq i64 0, %23 
  %25 = load  i8*, i8** %21, align 8 
  %26 = bitcast i8* %25 to i8* 
  %27 = and i1 1, 1 
  %28 = and i1 %24, %27 
  br i1 %28, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %29 = bitcast i8* %5 to {i64, i8*}* 
  %30 = getelementptr  {i64, i8*}, {i64, i8*}* %29, i32 0, i32 1 
  %31 = load  i8*, i8** %30, align 8 
  %32 = bitcast i8* %31 to i8* 
  %33 =  call ccc  i8*  @__234a9bccfad05393b28dc26cfaded70a__log(i8*  %32)  
  %34 = bitcast i8* %33 to i8* 
  br label %exitBlock_0 
exitBlock_0:
  %35 = phi i8* [%19, %branchExpBlock_0], [%34, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %36 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %37 = bitcast i8* %36 to double* 
  store  double 4.000000e0, double* %37, align 8 
  %38 = bitcast double* %37 to i8* 
  %39 =  call ccc  i8*  @__e9f54377746cb0755b8ab19b5a3e8824__Just(i8*  %38)  
  %40 = bitcast i8* %39 to i8* 
  %41 = bitcast i8* %40 to {i64, i8*}* 
  %42 = getelementptr  {i64, i8*}, {i64, i8*}* %41, i32 0, i32 1 
  %43 = getelementptr  {i64, i8*}, {i64, i8*}* %41, i32 0, i32 0 
  %44 = load  i64, i64* %43, align 8 
  %45 = icmp eq i64 0, %44 
  %46 = load  i8*, i8** %42, align 8 
  %47 = bitcast i8* %46 to double* 
  %48 = load  double, double* %47, align 8 
  %49 = and i1 1, 1 
  %50 = and i1 %45, %49 
  br i1 %50, label %branchExpBlock_2, label %exitBlock_1 
branchExpBlock_2:
  %51 = bitcast i8* %40 to {i64, i8*}* 
  %52 = getelementptr  {i64, i8*}, {i64, i8*}* %51, i32 0, i32 1 
  %53 = load  i8*, i8** %52, align 8 
  %54 = bitcast i8* %53 to double* 
  %55 = load  double, double* %54, align 8 
  %56 = fadd double %55, 1.000000e0 
  %57 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %58 = bitcast i8* %57 to double* 
  store  double %56, double* %58, align 8 
  %59 = bitcast double* %58 to i8* 
  br label %exitBlock_1 
exitBlock_1:
  %60 = phi i8* [%59, %branchExpBlock_2], [zeroinitializer, %exitBlock_0] 
  %61 = bitcast i8* (i8*)* @__234a9bccfad05393b28dc26cfaded70a__log to i8* 
  %62 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32}* getelementptr inbounds ({i8*, i32, i32}, {i8*, i32, i32}* inttoptr (i32 0 to {i8*, i32, i32}*), i32 1) to i64))  
  %63 = bitcast i8* %62 to {i8*, i32, i32}* 
  %64 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %63, i32 0, i32 0 
  store  i8* %61, i8** %64, align 8 
  %65 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %63, i32 0, i32 1 
  store  i32 1, i32* %65, align 8 
  %66 = getelementptr  {i8*, i32, i32}, {i8*, i32, i32}* %63, i32 0, i32 2 
  store  i32 1, i32* %66, align 8 
  %67 =  call ccc  i8*  @GC_malloc(i64  2)  
  %68 = getelementptr  i8, i8* %67, i32 0 
  store  i8 51, i8* %68, align 8 
  %69 = getelementptr  i8, i8* %67, i32 1 
  store  i8 0, i8* %69, align 8 
  %70 =  call ccc  {i8*, i8*}*  @MadList_singleton(i8*  %67)  
  %71 =  call ccc  i8*  @GC_malloc(i64  2)  
  %72 = getelementptr  i8, i8* %71, i32 0 
  store  i8 50, i8* %72, align 8 
  %73 = getelementptr  i8, i8* %71, i32 1 
  store  i8 0, i8* %73, align 8 
  %74 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %71, {i8*, i8*}*  %70)  
  %75 =  call ccc  i8*  @GC_malloc(i64  2)  
  %76 = getelementptr  i8, i8* %75, i32 0 
  store  i8 49, i8* %76, align 8 
  %77 = getelementptr  i8, i8* %75, i32 1 
  store  i8 0, i8* %77, align 8 
  %78 =  call ccc  {i8*, i8*}*  @__MadList_push__(i8*  %75, {i8*, i8*}*  %74)  
  %79 = bitcast {i8*, i8*}* %78 to {i8*, i8*}* 
  %80 = bitcast {i8*, i32, i32}* %63 to i8* 
  %81 = bitcast {i8*, i8*}* %79 to i8* 
  %82 =  call ccc  i8*  @$Functor$List$map(i8*  %80, i8*  %81)  
  %83 = bitcast i8* %82 to {i8*, i8*}* 
  %84 = load  double, double* @__4282899fd67a43a38f13e2be62dba7a9__NUM_EXPORTED, align 8 
  %85 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %86 = bitcast i8* %85 to double* 
  store  double %84, double* %86, align 8 
  %87 = bitcast double* %86 to i8* 
  %88 =  call ccc  i8*  @__234a9bccfad05393b28dc26cfaded70a__showNumber(i8*  %87)  
  %89 = bitcast i8* %88 to i8* 
  %90 =  call ccc  i8*  @__234a9bccfad05393b28dc26cfaded70a__log(i8*  %89)  
  %91 = bitcast i8* %90 to i8* 
  ret void 
}