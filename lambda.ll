; ModuleID = 'main'


 


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


@fst =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @fst$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @fst$fn(i8*  %env_0, i8*  %t_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %t_0 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %5 = load  i8*, i8** %3, align 8 
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = and i1 1, 1 
  %8 = load  i8*, i8** %4, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 %7, 1 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %14 = load  i8*, i8** %13, align 8 
  br label %exitBlock_0 
exitBlock_0:
  %15 = phi i8* [%12, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %15 
}


@snd =    global {i8* (i8*, i8*)*, {}*} { i8* (i8*, i8*)* @snd$fn, {}* @$EMPTY_ENV }


define external ccc  i8* @snd$fn(i8*  %env_0, i8*  %t_0)    {
; <label>:0:
  %1 = bitcast i8* %env_0 to {}* 
  %2 = bitcast i8* %t_0 to {i8*, i8*}* 
  %3 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %4 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %5 = load  i8*, i8** %3, align 8 
  %6 = bitcast i8* %5 to {i8*, i8*}* 
  %7 = and i1 1, 1 
  %8 = load  i8*, i8** %4, align 8 
  %9 = bitcast i8* %8 to {i8*, i8*}* 
  %10 = and i1 %7, 1 
  br i1 %10, label %branchExpBlock_0, label %exitBlock_0 
branchExpBlock_0:
  %11 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 0 
  %12 = load  i8*, i8** %11, align 8 
  %13 = getelementptr  {i8*, i8*}, {i8*, i8*}* %2, i32 0, i32 1 
  %14 = load  i8*, i8** %13, align 8 
  br label %exitBlock_0 
exitBlock_0:
  %15 = phi i8* [%14, %branchExpBlock_0], [zeroinitializer, %0] 
  ret i8* %15 
}


declare external ccc  i32 @puts(i8*)    


declare external ccc  i8* @malloc(i64)    


declare external ccc  i8* @GC_malloc(i64)    


declare external ccc  i8* @calloc(i32, i32)    


declare external ccc  i1 @__streq__(i8*, i8*)    


@$EMPTY_ENV =    global {} {  }


define external ccc  void @main()    {
entry_0:
  %0 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({double, double}* getelementptr inbounds ({double, double}, {double, double}* inttoptr (i32 0 to {double, double}*), i32 1) to i64))  
  %1 = bitcast i8* %0 to {double, double}* 
  %2 = getelementptr  {double, double}, {double, double}* %1, i32 0, i32 0 
  store  double 1.000000e0, double* %2, align 8 
  %3 = getelementptr  {double, double}, {double, double}* %1, i32 0, i32 1 
  store  double 2.000000e0, double* %3, align 8 
  %4 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({{double, double}*, double, double}* getelementptr inbounds ({{double, double}*, double, double}, {{double, double}*, double, double}* inttoptr (i32 0 to {{double, double}*, double, double}*), i32 1) to i64))  
  %5 = bitcast i8* %4 to {{double, double}*, double, double}* 
  %6 = getelementptr  {{double, double}*, double, double}, {{double, double}*, double, double}* %5, i32 0, i32 0 
  store  {double, double}* %1, {double, double}** %6, align 8 
  %7 = getelementptr  {{double, double}*, double, double}, {{double, double}*, double, double}* %5, i32 0, i32 1 
  store  double 3.000000e0, double* %7, align 8 
  %8 = getelementptr  {{double, double}*, double, double}, {{double, double}*, double, double}* %5, i32 0, i32 2 
  store  double 4.000000e0, double* %8, align 8 
  %9 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (double* getelementptr inbounds (double, double* inttoptr (i32 0 to double*), i32 1) to i64))  
  %10 = bitcast i8* %9 to double* 
  store  double 3.000000e0, double* %10, align 8 
  %11 = bitcast double* %10 to i8* 
  %12 = bitcast {i8* (i8*, i8*)*, {}*}* @Just to {i8* (i8*, i8*)*, i8*}* 
  %13 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %12, i32 0, i32 0 
  %14 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %13, align 8 
  %15 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %12, i32 0, i32 1 
  %16 = load  i8*, i8** %15, align 8 
  %17 =  call ccc  i8*  %14(i8*  %16, i8*  %11)  
  %18 = bitcast i8* %17 to {i64}* 
  %19 = getelementptr  {i64}, {i64}* %18, i32 0, i32 0 
  %20 = load  i64, i64* %19, align 8 
  %21 = icmp eq i64 1, %20 
  %22 = and i1 %21, 1 
  br i1 %22, label %branchExpBlock_0, label %nextBlock_0 
branchExpBlock_0:
  %23 = bitcast i8* %17 to {i64}* 
  %24 =  call ccc  i8*  @GC_malloc(i64  13)  
  %25 = getelementptr  i8, i8* %24, i32 0 
  store  i8 73, i8* %25, align 8 
  %26 = getelementptr  i8, i8* %24, i32 1 
  store  i8 32, i8* %26, align 8 
  %27 = getelementptr  i8, i8* %24, i32 2 
  store  i8 100, i8* %27, align 8 
  %28 = getelementptr  i8, i8* %24, i32 3 
  store  i8 111, i8* %28, align 8 
  %29 = getelementptr  i8, i8* %24, i32 4 
  store  i8 110, i8* %29, align 8 
  %30 = getelementptr  i8, i8* %24, i32 5 
  store  i8 39, i8* %30, align 8 
  %31 = getelementptr  i8, i8* %24, i32 6 
  store  i8 116, i8* %31, align 8 
  %32 = getelementptr  i8, i8* %24, i32 7 
  store  i8 32, i8* %32, align 8 
  %33 = getelementptr  i8, i8* %24, i32 8 
  store  i8 107, i8* %33, align 8 
  %34 = getelementptr  i8, i8* %24, i32 9 
  store  i8 110, i8* %34, align 8 
  %35 = getelementptr  i8, i8* %24, i32 10 
  store  i8 111, i8* %35, align 8 
  %36 = getelementptr  i8, i8* %24, i32 11 
  store  i8 119, i8* %36, align 8 
  %37 = getelementptr  i8, i8* %24, i32 12 
  store  i8 0, i8* %37, align 8 
  br label %exitBlock_0 
nextBlock_0:
  %38 = bitcast i8* %17 to {i64, i8*}* 
  %39 = getelementptr  {i64, i8*}, {i64, i8*}* %38, i32 0, i32 1 
  %40 = getelementptr  {i64, i8*}, {i64, i8*}* %38, i32 0, i32 0 
  %41 = load  i64, i64* %40, align 8 
  %42 = icmp eq i64 0, %41 
  %43 = load  i8*, i8** %39, align 8 
  %44 = bitcast i8* %43 to double* 
  %45 = load  double, double* %44, align 8 
  %46 = fcmp oeq double 3.000000e0, %45 
  %47 = and i1 1, %46 
  %48 = and i1 %42, %47 
  br i1 %48, label %branchExpBlock_1, label %exitBlock_0 
branchExpBlock_1:
  %49 = bitcast i8* %17 to {i64, i8*}* 
  %50 = getelementptr  {i64, i8*}, {i64, i8*}* %49, i32 0, i32 1 
  %51 = load  i8*, i8** %50, align 8 
  %52 =  call ccc  i8*  @GC_malloc(i64  2)  
  %53 = getelementptr  i8, i8* %52, i32 0 
  store  i8 118, i8* %53, align 8 
  %54 = getelementptr  i8, i8* %52, i32 1 
  store  i8 0, i8* %54, align 8 
  br label %exitBlock_0 
exitBlock_0:
  %55 = phi i8* [%24, %branchExpBlock_0], [%52, %branchExpBlock_1], [zeroinitializer, %nextBlock_0] 
  %56 =  call ccc  i32  @puts(i8*  %55)  
  %57 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({double, double}* getelementptr inbounds ({double, double}, {double, double}* inttoptr (i32 0 to {double, double}*), i32 1) to i64))  
  %58 = bitcast i8* %57 to {double, double}* 
  %59 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  store  double 1.000000e0, double* %59, align 8 
  %60 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  store  double 1.000000e0, double* %60, align 8 
  %61 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  %62 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  %63 = load  double, double* %61, align 8 
  %64 = bitcast double %63 to double* 
  %65 = load  double, double* %64, align 8 
  %66 = fcmp oeq double 2.000000e0, %65 
  %67 = and i1 1, %66 
  %68 = load  double, double* %62, align 8 
  %69 = bitcast double %68 to {double, double}* 
  %70 = and i1 %67, 1 
  br i1 %70, label %branchExpBlock_2, label %nextBlock_1 
branchExpBlock_2:
  %71 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  %72 = load  double, double* %71, align 8 
  %73 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  %74 = load  double, double* %73, align 8 
  %75 =  call ccc  i8*  @GC_malloc(i64  2)  
  %76 = getelementptr  i8, i8* %75, i32 0 
  store  i8 50, i8* %76, align 8 
  %77 = getelementptr  i8, i8* %75, i32 1 
  store  i8 0, i8* %77, align 8 
  br label %exitBlock_1 
nextBlock_1:
  %78 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  %79 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  %80 = load  double, double* %78, align 8 
  %81 = bitcast double %80 to double* 
  %82 = load  double, double* %81, align 8 
  %83 = fcmp oeq double 1.000000e0, %82 
  %84 = and i1 1, %83 
  %85 = load  double, double* %79, align 8 
  %86 = bitcast double %85 to double* 
  %87 = load  double, double* %86, align 8 
  %88 = fcmp oeq double 2.000000e0, %87 
  %89 = and i1 %84, %88 
  br i1 %89, label %branchExpBlock_3, label %nextBlock_2 
branchExpBlock_3:
  %90 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  %91 = load  double, double* %90, align 8 
  %92 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  %93 = load  double, double* %92, align 8 
  %94 =  call ccc  i8*  @GC_malloc(i64  2)  
  %95 = getelementptr  i8, i8* %94, i32 0 
  store  i8 49, i8* %95, align 8 
  %96 = getelementptr  i8, i8* %94, i32 1 
  store  i8 0, i8* %96, align 8 
  br label %exitBlock_1 
nextBlock_2:
  %97 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  %98 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  %99 = load  double, double* %97, align 8 
  %100 = bitcast double %99 to double* 
  %101 = load  double, double* %100, align 8 
  %102 = fcmp oeq double 1.000000e0, %101 
  %103 = and i1 1, %102 
  %104 = load  double, double* %98, align 8 
  %105 = bitcast double %104 to double* 
  %106 = load  double, double* %105, align 8 
  %107 = fcmp oeq double 1.000000e0, %106 
  %108 = and i1 %103, %107 
  br i1 %108, label %branchExpBlock_4, label %exitBlock_1 
branchExpBlock_4:
  %109 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 0 
  %110 = load  double, double* %109, align 8 
  %111 = getelementptr  {double, double}, {double, double}* %58, i32 0, i32 1 
  %112 = load  double, double* %111, align 8 
  %113 =  call ccc  i8*  @GC_malloc(i64  10)  
  %114 = getelementptr  i8, i8* %113, i32 0 
  store  i8 49, i8* %114, align 8 
  %115 = getelementptr  i8, i8* %113, i32 1 
  store  i8 32, i8* %115, align 8 
  %116 = getelementptr  i8, i8* %113, i32 2 
  store  i8 97, i8* %116, align 8 
  %117 = getelementptr  i8, i8* %113, i32 3 
  store  i8 110, i8* %117, align 8 
  %118 = getelementptr  i8, i8* %113, i32 4 
  store  i8 100, i8* %118, align 8 
  %119 = getelementptr  i8, i8* %113, i32 5 
  store  i8 32, i8* %119, align 8 
  %120 = getelementptr  i8, i8* %113, i32 6 
  store  i8 49, i8* %120, align 8 
  %121 = getelementptr  i8, i8* %113, i32 7 
  store  i8 33, i8* %121, align 8 
  %122 = getelementptr  i8, i8* %113, i32 8 
  store  i8 33, i8* %122, align 8 
  %123 = getelementptr  i8, i8* %113, i32 9 
  store  i8 0, i8* %123, align 8 
  br label %exitBlock_1 
exitBlock_1:
  %124 = phi i8* [%75, %branchExpBlock_2], [%94, %branchExpBlock_3], [%113, %branchExpBlock_4], [zeroinitializer, %nextBlock_2] 
  %125 =  call ccc  i32  @puts(i8*  %124)  
  %126 =  call ccc  i8*  @GC_malloc(i64  6)  
  %127 = getelementptr  i8, i8* %126, i32 0 
  store  i8 102, i8* %127, align 8 
  %128 = getelementptr  i8, i8* %126, i32 1 
  store  i8 105, i8* %128, align 8 
  %129 = getelementptr  i8, i8* %126, i32 2 
  store  i8 114, i8* %129, align 8 
  %130 = getelementptr  i8, i8* %126, i32 3 
  store  i8 115, i8* %130, align 8 
  %131 = getelementptr  i8, i8* %126, i32 4 
  store  i8 116, i8* %131, align 8 
  %132 = getelementptr  i8, i8* %126, i32 5 
  store  i8 0, i8* %132, align 8 
  %133 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, double}* getelementptr inbounds ({i8*, double}, {i8*, double}* inttoptr (i32 0 to {i8*, double}*), i32 1) to i64))  
  %134 = bitcast i8* %133 to {i8*, double}* 
  %135 = getelementptr  {i8*, double}, {i8*, double}* %134, i32 0, i32 0 
  store  i8* %126, i8** %135, align 8 
  %136 = getelementptr  {i8*, double}, {i8*, double}* %134, i32 0, i32 1 
  store  double 2.000000e0, double* %136, align 8 
  %137 = bitcast {i8*, double}* %134 to i8* 
  %138 = bitcast {i8* (i8*, i8*)*, {}*}* @fst to {i8* (i8*, i8*)*, i8*}* 
  %139 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %138, i32 0, i32 0 
  %140 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %139, align 8 
  %141 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %138, i32 0, i32 1 
  %142 = load  i8*, i8** %141, align 8 
  %143 =  call ccc  i8*  %140(i8*  %142, i8*  %137)  
  %144 =  call ccc  i8*  @GC_malloc(i64  6)  
  %145 = getelementptr  i8, i8* %144, i32 0 
  store  i8 102, i8* %145, align 8 
  %146 = getelementptr  i8, i8* %144, i32 1 
  store  i8 105, i8* %146, align 8 
  %147 = getelementptr  i8, i8* %144, i32 2 
  store  i8 114, i8* %147, align 8 
  %148 = getelementptr  i8, i8* %144, i32 3 
  store  i8 115, i8* %148, align 8 
  %149 = getelementptr  i8, i8* %144, i32 4 
  store  i8 116, i8* %149, align 8 
  %150 = getelementptr  i8, i8* %144, i32 5 
  store  i8 0, i8* %150, align 8 
  %151 =  call ccc  i8*  @GC_malloc(i64  7)  
  %152 = getelementptr  i8, i8* %151, i32 0 
  store  i8 115, i8* %152, align 8 
  %153 = getelementptr  i8, i8* %151, i32 1 
  store  i8 101, i8* %153, align 8 
  %154 = getelementptr  i8, i8* %151, i32 2 
  store  i8 99, i8* %154, align 8 
  %155 = getelementptr  i8, i8* %151, i32 3 
  store  i8 111, i8* %155, align 8 
  %156 = getelementptr  i8, i8* %151, i32 4 
  store  i8 110, i8* %156, align 8 
  %157 = getelementptr  i8, i8* %151, i32 5 
  store  i8 100, i8* %157, align 8 
  %158 = getelementptr  i8, i8* %151, i32 6 
  store  i8 0, i8* %158, align 8 
  %159 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %160 = bitcast i8* %159 to {i8*, i8*}* 
  %161 = getelementptr  {i8*, i8*}, {i8*, i8*}* %160, i32 0, i32 0 
  store  i8* %144, i8** %161, align 8 
  %162 = getelementptr  {i8*, i8*}, {i8*, i8*}* %160, i32 0, i32 1 
  store  i8* %151, i8** %162, align 8 
  %163 = bitcast {i8*, i8*}* %160 to i8* 
  %164 = bitcast {i8* (i8*, i8*)*, {}*}* @snd to {i8* (i8*, i8*)*, i8*}* 
  %165 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %164, i32 0, i32 0 
  %166 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %165, align 8 
  %167 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %164, i32 0, i32 1 
  %168 = load  i8*, i8** %167, align 8 
  %169 =  call ccc  i8*  %166(i8*  %168, i8*  %163)  
  %170 =  call ccc  i8*  @GC_malloc(i64  6)  
  %171 = getelementptr  i8, i8* %170, i32 0 
  store  i8 116, i8* %171, align 8 
  %172 = getelementptr  i8, i8* %170, i32 1 
  store  i8 104, i8* %172, align 8 
  %173 = getelementptr  i8, i8* %170, i32 2 
  store  i8 105, i8* %173, align 8 
  %174 = getelementptr  i8, i8* %170, i32 3 
  store  i8 114, i8* %174, align 8 
  %175 = getelementptr  i8, i8* %170, i32 4 
  store  i8 100, i8* %175, align 8 
  %176 = getelementptr  i8, i8* %170, i32 5 
  store  i8 0, i8* %176, align 8 
  %177 =  call ccc  i8*  @GC_malloc(i64  1)  
  %178 = getelementptr  i8, i8* %177, i32 0 
  store  i8 0, i8* %178, align 8 
  %179 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*}* getelementptr inbounds ({i8*, i8*}, {i8*, i8*}* inttoptr (i32 0 to {i8*, i8*}*), i32 1) to i64))  
  %180 = bitcast i8* %179 to {i8*, i8*}* 
  %181 = getelementptr  {i8*, i8*}, {i8*, i8*}* %180, i32 0, i32 0 
  store  i8* %170, i8** %181, align 8 
  %182 = getelementptr  {i8*, i8*}, {i8*, i8*}* %180, i32 0, i32 1 
  store  i8* %177, i8** %182, align 8 
  %183 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({double, {i8*, i8*}*}* getelementptr inbounds ({double, {i8*, i8*}*}, {double, {i8*, i8*}*}* inttoptr (i32 0 to {double, {i8*, i8*}*}*), i32 1) to i64))  
  %184 = bitcast i8* %183 to {double, {i8*, i8*}*}* 
  %185 = getelementptr  {double, {i8*, i8*}*}, {double, {i8*, i8*}*}* %184, i32 0, i32 0 
  store  double 1.000000e0, double* %185, align 8 
  %186 = getelementptr  {double, {i8*, i8*}*}, {double, {i8*, i8*}*}* %184, i32 0, i32 1 
  store  {i8*, i8*}* %180, {i8*, i8*}** %186, align 8 
  %187 = bitcast {double, {i8*, i8*}*}* %184 to i8* 
  %188 = bitcast {i8* (i8*, i8*)*, {}*}* @snd to {i8* (i8*, i8*)*, i8*}* 
  %189 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %188, i32 0, i32 0 
  %190 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %189, align 8 
  %191 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %188, i32 0, i32 1 
  %192 = load  i8*, i8** %191, align 8 
  %193 =  call ccc  i8*  %190(i8*  %192, i8*  %187)  
  %194 = bitcast {i8* (i8*, i8*)*, {}*}* @fst to {i8* (i8*, i8*)*, i8*}* 
  %195 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %194, i32 0, i32 0 
  %196 = load  i8* (i8*, i8*)*, i8* (i8*, i8*)** %195, align 8 
  %197 = getelementptr  {i8* (i8*, i8*)*, i8*}, {i8* (i8*, i8*)*, i8*}* %194, i32 0, i32 1 
  %198 = load  i8*, i8** %197, align 8 
  %199 =  call ccc  i8*  %196(i8*  %198, i8*  %193)  
  %200 =  call ccc  i32  @puts(i8*  %143)  
  %201 =  call ccc  i32  @puts(i8*  %169)  
  %202 =  call ccc  i32  @puts(i8*  %199)  
  ret void 
}