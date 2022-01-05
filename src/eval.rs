use crate::{
  expression::*,
  value::*,
};
use sp_std::{
  boxed::Box,
  rc::Rc,
};
use tailcall::trampoline;


pub type Continuation = Option<Box<Node>>;

pub struct Node {
  term: Rc<Expression>,
  env: Env,
  args: Args,
  cont: Continuation,
}

pub fn eval(heap: &mut Heap, term: Rc<Expression>, env: Env, args: Args) -> ValuePtr {
  trampoline::run(eval_step, (heap, term, env, args, None))
}

#[inline(always)]
fn cont_or_ret<'a>(
  heap: &'a mut Heap, val: ValuePtr, cont: Continuation
) -> trampoline::Next<(&'a mut Heap, Rc<Expression>, Env, Args, Continuation), ValuePtr> {
  match cont {
    None => trampoline::Finish(val),
    Some(mut ctx) => {
      ctx.args.push(val);
      trampoline::Recurse((heap, ctx.term, ctx.env, ctx.args, ctx.cont))
    },
  }
}

#[inline(always)]
pub fn eval_step<'a>(
  (heap, term, mut env, mut args, mut cont): (&'a mut Heap, Rc<Expression>, Env, Args, Continuation)
) -> trampoline::Next<(&'a mut Heap, Rc<Expression>, Env, Args, Continuation), ValuePtr> {
  match &*term {
    Expression::App(_, fun, arg) => {
      cont = Some(
        Box::new(Node {
          term: fun.clone(),
          env: env.clone(),
          args,
          cont,
        })
      );
      trampoline::Recurse((heap, arg.clone(), env, vec![], cont))
    },
    Expression::Lam(_, nam, bnd, _, bod) => {
      match args.pop() {
        Some(arg) => {
          env.push_front(arg);
          trampoline::Recurse((heap, bod.clone(), env, args, cont))
        },
        None => {
          let val = lam(nam.clone(), bnd.clone(), bod.clone(), env, heap);
	  cont_or_ret(heap, val, cont)
        },
      }
    },
    Expression::Var(_, idx) => {
      let val = env[*idx];
      if args.is_empty() {
	cont_or_ret(heap, val, cont)
      }
      else {
        match &heap[val as usize] {
          Value::App(tuple) => {
            let (neu, p_args) = &**tuple;
            args.extend_from_slice(p_args);
	    match neu {
	      Neutral::Var(idx) => {
		let val = app(Neutral::Var(*idx), args, heap);
		cont_or_ret(heap, val, cont)
	      },
	      Neutral::Const(..) => {
		// Have to check what kind of constant it is. A recursor for instance, could reduce after extending `args`
		todo!()
	      },
	    }
          }
          Value::Lam(tuple) => {
	    let (_, _, bod, env) = &**tuple;
            let mut env = env.clone();
            env.push_front(args.pop().unwrap());
	    let term = bod.clone();
            trampoline::Recurse((heap, term, env, args, cont))
          },
	  // Should not be possible since expressions are well-typed
	  _ => unreachable!(),
        }
      }
    },
    _ => todo!()
  }
}
