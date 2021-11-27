define external ccc  i8* @fibHelper$lifted$1(i8*  %$Number$p41_0, i8*  %$Number$w22_0, i8*  %a_0, i8*  %b_0, i8*  %counter_0)    {
; <label>:0:
  %1 = bitcast i8* %$Number$p41_0 to i8* 
  %2 = bitcast i8* %$Number$w22_0 to i8* 
  %3 = bitcast i8* %a_0 to i8* 
  %4 = bitcast i8* %b_0 to i8* 
  %5 = bitcast i8* %counter_0 to i8* 
  %6 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %7 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %6, i32 0, i32 5 
  %8 = bitcast {i8*, i32, i32, i8*}* %7 to i8* 
  %9 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %10 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %9, i32 0, i32 7 
  %11 = bitcast {i8*, i32, i32, i8*}* %10 to i8* 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %13 = bitcast i8* %12 to i64* 
  store  i64 0, i64* %13, align 8 
  %14 = bitcast i64* %13 to i8* 
  %15 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %11, i32  1, i8*  %14)  
  %16 = bitcast i8* %15 to i8* 
  %17 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %8, i32  2, i8*  %counter_0, i8*  %15)  
  %18 = bitcast i8* %17 to i1* 
  %19 = load  i1, i1* %18, align 8 
  br i1 %19, label %if.then_0, label %if.else_0 
if.then_0:
  %20 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %21 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %20, i32 0, i32 1 
  %22 = bitcast {i8*, i32, i32, i8*}* %21 to i8* 
  %23 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %22, i32  2, i8*  %a_0, i8*  %b_0)  
  %24 = bitcast i8* %23 to i8* 
  %25 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %26 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %25, i32 0, i32 2 
  %27 = bitcast {i8*, i32, i32, i8*}* %26 to i8* 
  %28 = bitcast i8* %2 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %29 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %28, i32 0, i32 7 
  %30 = bitcast {i8*, i32, i32, i8*}* %29 to i8* 
  %31 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %32 = bitcast i8* %31 to i64* 
  store  i64 1, i64* %32, align 8 
  %33 = bitcast i64* %32 to i8* 
  %34 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %30, i32  1, i8*  %33)  
  %35 = bitcast i8* %34 to i8* 
  %36 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %27, i32  2, i8*  %counter_0, i8*  %34)  
  %37 = bitcast i8* %36 to i8* 
  %38 =  call ccc  i8*  @fibHelper$lifted$1(i8*  %$Number$p41_0, i8*  %$Number$w22_0, i8*  %b_0, i8*  %23, i8*  %36)  
  %39 = bitcast i8* %38 to i8* 
  br label %if.exit_0 
if.else_0:
  br label %if.exit_0 
if.exit_0:
  %40 = phi i8* [%38, %if.then_0], [%a_0, %if.else_0] 
  ret i8* %40 
}