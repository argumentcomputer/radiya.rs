use crate::{
  declaration::{
    ConstantInfo,
    QuotKind,
    QuotVal,
  },
  environment::Environment,
  expression::{
    BinderInfo,
    Expr,
  },
  local_context::{
    LocalContext,
    LocalDecl,
  },
  name,
  name::{
    Name,
    NameGenerator,
  },
  universe::Univ,
};
use alloc::string::String;
use num_bigint::BigUint;
use num_traits::Zero;
use sp_im::{
  vector,
  Vector,
};
use sp_std::rc::Rc;

/// Quotient types

pub mod quot_consts {
  use crate::name::Name;
  // pub const g_quot: Name = Name::simple(&["Quot"]);
  // pub const g_quot_lift: Name = Name::simple(&["Quot", "lift"]);
  // pub const g_quot_ind: Name = Name::simple(&["Quot", "ind"]);
  // pub const g_quot_mk: Name = Name::simple(&["Quot", "mk"]);
}

pub fn add_quot(env: &Environment) -> Result<(), String> {
  let eq = name!("Eq");
  let eq_info = env.get(&eq).ok_or("env does not have Eq constant")?;

  match eq_info {
    ConstantInfo::Inductive(eq_val) => {
      let mut local_context = LocalContext::default();
      let name_gen = NameGenerator::new(Name::empty());
      let sort_u = Expr::Sort(Univ::Param(name!("u")));
      let sort_v = Expr::Sort(Univ::Param(name!("v")));
      let alpha_decl = local_context.add_local_decl_with_binding(
        name!("α"),
        name!("a"),
        sort_u.clone(),
        BinderInfo::Implicit,
      );
      let alpha = Expr::FVar(name!("α"));
      let quot_r = Expr::Pi {
        name: name!("r"),
        binder_info: BinderInfo::Default,
        from: Rc::new(alpha.clone()),
        to: Rc::new(alpha.clone()),
      };
      let quot_typ = Expr::Pi {
        name: Name::empty(),
        binder_info: BinderInfo::Implicit,
        from: Rc::new(alpha),
        to: Rc::new(Expr::Pi {
          name: Name::empty(),
          binder_info: BinderInfo::Default,
          from: Rc::new(quot_r),
          to: Rc::new(sort_u.clone()),
        }),
      };

      // constant {u} quot {α : Sort u} (r : α → α → Prop) : Sort u
      let quot_info = ConstantInfo::Quot(QuotVal {
        name: name!("quot"),
        level_params: vector![name!("u")],
        typ: quot_typ,
        kind: QuotKind::Type,
      });
      // new_env.add_core(constant_info(quot_val(*quot_consts::g_quot, {u_name},
      // lctx.mk_pi({alpha, r}, Sort_u), quot_kind::Type))); expr quot_r
      // = mk_app(mk_constant(*quot_consts::g_quot, {u}), alpha, r);
      // expr a          = lctx.mk_local_decl(g, "a", alpha);
      // constant {u} quot.mk {α : Sort u} (r : α → α → Prop) (a : α) : @quot.{u} α r
      // new_env.add_core(constant_info(quot_val(*quot_consts::g_quot_mk, {u_name},
      // lctx.mk_pi({alpha, r, a}, quot_r), quot_kind::Mk)));
      // make r implicit
      // lctx = local_ctx();
      // alpha           = lctx.mk_local_decl(g, "α", Sort_u,
      // mk_implicit_binder_info()); r               =
      // lctx.mk_local_decl(g, "r", mk_arrow(alpha, mk_arrow(alpha, mk_Prop())),
      // mk_implicit_binder_info()); quot_r          =
      // mk_app(mk_constant(*quot_consts::g_quot, {u}), alpha, r); a
      // = lctx.mk_local_decl(g, "a", alpha); name v_name("v");
      // level v         = mk_univ_param(v_name);
      // expr Sort_v     = mk_sort(v);
      let beta = local_context.add_local_decl_with_binding(
        name!("β"),
        name!("b"),
        sort_u,
        BinderInfo::Implicit,
      );
      // expr beta       = lctx.mk_local_decl(g, "β", Sort_v,
      // mk_implicit_binder_info()); expr f          =
      // lctx.mk_local_decl(g, "f", mk_arrow(alpha, beta)); expr b
      // = lctx.mk_local_decl(g, "b", alpha); expr r_a_b      = mk_app(r,
      // a, b);
      // f a = f b
      // expr f_a_eq_f_b = mk_app(mk_constant("Eq", {v}), beta, mk_app(f, a),
      // mk_app(f, b));
      // (∀ a b : α, r a b → f a = f b)
      // expr sanity     = lctx.mk_pi({a, b}, mk_arrow(r_a_b, f_a_eq_f_b));
      // constant {u v} quot.lift {α : Sort u} {r : α → α → Prop} {β : Sort v} (f : α
      // → β) : (∀ a b : α, r a b → f a = f b) →  @quot.{u} α r → β
      // new_env.add_core(constant_info(quot_val(*quot_consts::g_quot_lift, {u_name,
      // v_name},
      // lctx.mk_pi({alpha, r, beta, f}, mk_arrow(sanity, mk_arrow(quot_r, beta))),
      // quot_kind::Lift)));
      // {β : @quot.{u} α r → Prop}
      // beta            = lctx.mk_local_decl(g, "β", mk_arrow(quot_r, mk_Prop()),
      // mk_implicit_binder_info()); expr quot_mk_a  =
      // mk_app(mk_constant(*quot_consts::g_quot_mk, {u}), alpha, r, a);
      // expr all_quot   = lctx.mk_pi(a, mk_app(beta, quot_mk_a));
      // expr q          = lctx.mk_local_decl(g, "q", quot_r);
      // expr beta_q     = mk_app(beta, q);
      // constant {u} quot.ind {α : Sort u} {r : α → α → Prop} {β : @quot.{u} α r →
      // Prop} : (∀ a : α, β (@quot.mk.{u} α r a)) → ∀ q : @quot.{u} α r,
      // β q
      // new_env.add_core(constant_info(quot_val(*quot_consts::g_quot_ind, {u_name},
      //                                         lctx.mk_pi({alpha, r, beta},
      // mk_pi("mk", all_quot, lctx.mk_pi(q, beta_q))), quot_kind::Ind)));

      Ok(())
    }
    _ => Err(format!(
      "failed to initialize quot module,environment does not have an inductive 'Eq' type"
    )),
  }
}
