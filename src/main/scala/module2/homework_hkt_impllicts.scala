package module2

object homework_hkt_impllicts{

    /**
      * 
      * Доработать сигнатуру tupleF и реализовать его
      * По итогу должны быть возможны подобные вызовы
      *   val r1 = println(tupleF(optA, optB))
      *   val r2 = println(tupleF(list1, list2))
      * 
      */
    implicit def tupleF[F[_], A, B](fa: F[A], fb: F[B])(implicit bindable: F[A] => Bindable[F, A],
                                               bindable2: F[B] => Bindable[F, B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))


    trait Bindable[F[_], A] {
        def map[B](f: A => B): F[B]
        def flatMap[B](f: A => F[B]): F[B]
    }


   implicit def optBindable[A](opt: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
     def map[B](f: A => B): Option[B] = opt.map(f)
     def flatMap[B](f: A => Option[B]): Option[B] = opt.flatMap(f)
  }

  implicit def listBindable[A](list: List[A]): Bindable[List, A] = new Bindable[List, A] {
     def map[B](f: A => B): List[B] = list.map(f)
     def flatMap[B](f: A => List[B]): List[B] = list.flatMap(f)
  }


  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))
}