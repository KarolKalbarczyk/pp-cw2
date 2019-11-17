  let rec repeat n x (lazy tail)=
    if n!=0 then LCons(x, lazy( repeat (n-1) x (lazy tail)))
    else tail

  let rec lrepeat n xs = 
    let rec repeat n x (lazy tail)=
      if n!=0 then LCons(x, lazy( repeat (n-1) x (lazy tail)))
      else tail in
    match xs with
    LCons(h, lazy t) -> lazy(repeat n h (lrepeat n t))
    | LNil -> lazy(LNil)

    let lFib =
      let rec fib acc next =
        LCons(acc, lazy(fib next (acc+next))) in
      fib 0 1  


  type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT)    

  let lBreadth lbt =
  
    let rec breadth queue =
      match queue with 
      | [] -> LNil
      | h::t -> match h with
                LNode(a,l,r) -> LCons(a,lazy(breadth (l()::r()::t)))
                | LEmpty -> (breadth t) in
    match lbt with 
      LNode(_,_,_) -> breadth [lbt]
      | LEmpty -> LNil              

  
   let rec lTree n =
    LNode(n, (fun ()->lTree(2*n),(fun ()-> lTree(2*n+1)))
      def whileLoop(p: Any=>Boolean)(exp:Any=>Any): Unit ={
    if(p()){
      exp()
      whileLoop(p)(exp)
    }
  }

  def swap(arr:Array[Int],i:Int,j:Int): Unit ={
    val help = arr(j);
    arr(j) = arr(i);
    arr(i) = help;
  }

  def choosePivot(arr:Array[Int]): Int = {
    arr(arr.length/2)
  }

  def partition(arr:Array[Int],i:Int,j:Int): (Int,Int) ={
    var ii = i;
    var jj = j;
    val pivot = choosePivot(arr)
    while (ii<jj){
      while (arr(ii)<pivot) ii+=1
      while (arr(jj)>pivot) jj-=1
      if (ii<=jj) swap(arr,ii,jj); ii+=1;jj-=1
    }
    (ii,jj)
  }

  def quick(arr:Array[Int],l:Int,r:Int): Unit ={
    if (l<r) {
      var (i,j) = partition(arr,l,r)
      if (j-l<r-i){
        quick(arr,l,j)
        quick(arr,i,r)
      }else{
        quick(arr,i,r)
        quick(arr,l,j)
      }
    }else ()
  }

  def quicksort(arr:Array[Int]){quick(arr,0,arr.length-1)}






  /*#let quicksort tab = quick tab 0 ((Array.length tab)-1);;
  let rec quick tab l r =
    if l < r then
      let (i,j) = partition tab l r
      in if j-l < r-i (* usprawnienie 3 *)
  then let _ = quick tab l j in quick tab i r
  else let _ = quick tab i r in quick tab l j
  else ();;
  val quick : 'a array -> int -> int -> unit = <fun>*/



  def treeToLTree[A](t:BT[A]):lBT[A]={
    t match {
      case Empty => LEmpty
      case Node(elem, left, right) =>LNode(elem,()=>treeToLTree(left),()=>treeToLTree(right))
    }
  }

  val lFib:LazyList[Int] = {
    def fib(acc:Int,next:Int):LazyList[Int] ={
      acc#::fib(next,acc+next)
    }
    fib(0,1)
  }


  def lBreadth[A](ltree: lBT[A]):LazyList[A] ={
    def traverse(queue:List[lBT[A]]):LazyList[A] =
      queue match {
        case h::t=> h match {
          case LNode(elem, left, right) =>elem#::traverse(t:::List(left(),right()))
          case LEmpty => traverse(t)
        }
        case Nil =>LazyList.empty
      }
    traverse(List(ltree))
  }
  def lTree(n:Int):lBT[Int] = {
    LNode(n,() =>lTree(2*n), () =>lTree(2*n+1))
  }

  def lBTake[A](n:Int,t:lBT[A]):lBT[A] = {
    t match {
      case LNode(elem, left, right)if(n!=0) => LNode(elem,() =>lBTake(n-1,left()),() =>lBTake(n-1,right()))
      case _ => LEmpty
    }
  }

   def lrepeat[A](k: Int)(lxs: LazyList[A]):LazyList[A] ={
     def rep(x:A,k:Int): LazyList[A] ={
       if (k>0) x#::rep(x,k-1)
       else LazyList.empty
     }
     lxs match {
       case h#::t =>
         rep(h,k)#:::lrepeat(k)(t)
       case _ => LazyList.empty
     }
   }

 }
