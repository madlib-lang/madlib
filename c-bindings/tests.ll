define external ccc  i8* @$lambda$lifted$0(i8*  %$Number$g6_0, i8*  %a_0, i8*  %count_0, i8*  %__0)    {
  %1 = bitcast i8* %$Number$g6_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %count_0 to i8* 
  %4 = bitcast i8* %__0 to i8* 
  %5 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %6 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %5, i32 0, i32 1 
  %7 = bitcast {i8*, i32, i32, i8*}* %6 to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %7, i32  2, i8*  %3, i8*  %2)  
  %9 = bitcast i8* %8 to i64* 
  %10 = load  i64, i64* %9, align 8 
  %11 = bitcast i8* %count_0 to i64* 
  %12 = bitcast i64 %10 to i64 
  store  i64 %12, i64* %11, align 8 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %14 = bitcast i8* %13 to i64* 
  store  i64 %10, i64* %14, align 8 
  %15 = bitcast i64* %14 to i8* 
  ret i8* %15 
}










define external ccc  i8* @$lambda$lifted$0(i8*  %$Number$g6_0, i8*  %a_0, i8*  %count_0, i8*  %__0)    {
  %1 = bitcast i8* %$Number$g6_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %count_0 to i8* 
  %4 = bitcast i8* %__0 to i8* 
  %5 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %6 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %5, i32 0, i32 1 
  %7 = bitcast {i8*, i32, i32, i8*}* %6 to i8* 
  %8 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %7, i32  2, i8*  %3, i8*  %2)  
  %9 = bitcast i8* %8 to i64* 
  %10 = load  i64, i64* %9, align 8 
  %11 = bitcast i8* %count_0 to i64* 
  %12 = bitcast i64 %10 to i64 
  store  i64 %12, i64* %11, align 8 
  %13 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %14 = bitcast i8* %13 to i64* 
  store  i64 %10, i64* %14, align 8 
  %15 = bitcast i64* %14 to i8* 
  ret i8* %15 
}










define external ccc  i8* @$lambda$lifted$1(i8*  %$Number$g6_0, i8*  %a_0)    {
  %1 = bitcast i8* %$Number$g6_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %4 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %3, i32 0, i32 3 
  %5 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %7 = bitcast i8* %6 to i64* 
  store  i64 1, i64* %7, align 8 
  %8 = bitcast i64* %7 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  1, i8*  %8)  
  %10 = bitcast i8* %9 to i64* 
  %11 = load  i64, i64* %10, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
STORE-PTR: LocalReference
  PointerType
    { pointerReferent = IntegerType { typeBits = 64 }
    , pointerAddrSpace = AddrSpace 0
    }
  (UnName 13)
  %13 = bitcast i8* %12 to i64* 
  store  i64 %11, i64* %13, align 8 
  %14 = bitcast i8* (i8*, i8*, i8*, i8*)* @$lambda$lifted$0 to i8* 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %16 = bitcast i8* %15 to i64* 
  store  i64 %11, i64* %16, align 8 
  %17 = bitcast i64* %16 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*}, {i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {i8*, i8*, i8*}* 
  %20 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 0 
  store  i8* %1, i8** %20, align 8 
  %21 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 1 
  store  i8* %2, i8** %21, align 8 
  %22 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 2 
  store  i8* %17, i8** %22, align 8 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {i8*, i32, i32, i8*}* 
  %25 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 0 
  store  i8* %14, i8** %25, align 8 
  %26 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 1 
  store  i32 4, i32* %26, align 8 
  %27 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 2 
  store  i32 1, i32* %27, align 8 
  %28 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 3 
  store  i8* %18, i8** %28, align 8 
  %29 = bitcast {i8*, i32, i32, i8*}* %24 to i8* 
  ret i8* %29 
}


define external ccc  i8* @$lambda$lifted$1(i8*  %$Number$g6_0, i8*  %a_0)    {
  %1 = bitcast i8* %$Number$g6_0 to i8* 
  %2 = bitcast i8* %a_0 to i8* 
  %3 = bitcast i8* %1 to {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* 
  %4 = getelementptr  {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}, {{i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}}* %3, i32 0, i32 3 
  %5 = bitcast {i8*, i32, i32, i8*}* %4 to i8* 
  %6 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %7 = bitcast i8* %6 to i64* 
  store  i64 1, i64* %7, align 8 
  %8 = bitcast i64* %7 to i8* 
  %9 =  call ccc  i8* (i8*, i32, ...) @__applyPAP__(i8*  %5, i32  1, i8*  %8)  
  %10 = bitcast i8* %9 to i64* 
  %11 = load  i64, i64* %10, align 8 
  %12 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
STORE-PTR: LocalReference
  PointerType
    { pointerReferent = IntegerType { typeBits = 64 }
    , pointerAddrSpace = AddrSpace 0
    }
  (UnName 13)
  %13 = bitcast i8* %12 to i64* 
  store  i64 %11, i64* %13, align 8 
  %14 = bitcast i8* (i8*, i8*, i8*, i8*)* @$lambda$lifted$0 to i8* 
  %15 =  call ccc  i8*  @GC_malloc(i64  ptrtoint (i64* getelementptr inbounds (i64, i64* inttoptr (i32 0 to i64*), i32 1) to i64))  
  %16 = bitcast i8* %15 to i64* 
  store  i64 %11, i64* %16, align 8 
  %17 = bitcast i64* %16 to i8* 
  %18 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i8*, i8*}* getelementptr inbounds ({i8*, i8*, i8*}, {i8*, i8*, i8*}* inttoptr (i32 0 to {i8*, i8*, i8*}*), i32 1) to i64))  
  %19 = bitcast i8* %18 to {i8*, i8*, i8*}* 
  %20 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 0 
  store  i8* %1, i8** %20, align 8 
  %21 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 1 
  store  i8* %2, i8** %21, align 8 
  %22 = getelementptr  {i8*, i8*, i8*}, {i8*, i8*, i8*}* %19, i32 0, i32 2 
  store  i8* %17, i8** %22, align 8 
  %23 =  call ccc  i8*  @GC_malloc(i64  ptrtoint ({i8*, i32, i32, i8*}* getelementptr inbounds ({i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* inttoptr (i32 0 to {i8*, i32, i32, i8*}*), i32 1) to i64))  
  %24 = bitcast i8* %23 to {i8*, i32, i32, i8*}* 
  %25 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 0 
  store  i8* %14, i8** %25, align 8 
  %26 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 1 
  store  i32 4, i32* %26, align 8 
  %27 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 2 
  store  i32 1, i32* %27, align 8 
  %28 = getelementptr  {i8*, i32, i32, i8*}, {i8*, i32, i32, i8*}* %24, i32 0, i32 3 
  store  i8* %18, i8** %28, align 8 
  %29 = bitcast {i8*, i32, i32, i8*}* %24 to i8* 
  ret i8* %29 
}