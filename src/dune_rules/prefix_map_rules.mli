open Import

module All_sctx : sig
  val all_sctx_to_entries :
       Super_context.t Context_name.Map.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t Memo.t
end

module Build_path_prefix_map_dyn : sig
  val pair_to_dyn : Build_path_prefix_map.pair -> Dyn.t

  val to_dyn : Build_path_prefix_map.map -> Dyn.t
end

module Inverse_map : sig
  val invert_maps :
       Build_path_prefix_map.map String.Map.t
    -> Build_path_prefix_map.map String.Map.t
end

module Build_map : sig
  type t = Build_path_prefix_map.map String.Map.t

  val to_dyn : t -> Dyn.t

  val build_context_map_full :
       Super_context.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t
    -> Build_path_prefix_map.map

  val build_all_maps_full :
       Super_context.t Context_name.Map.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t
    -> Build_path_prefix_map.map String.Map.t

  val build_context_map :
       Super_context.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t
    -> Build_path_prefix_map.map

  val build_all_maps :
       Super_context.t Context_name.Map.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t
    -> Build_path_prefix_map.map String.Map.t

  val build_context_inverse_source_map :
       Super_context.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t
    -> Build_path_prefix_map.map

  val build_inverse_source_maps :
       Super_context.t Context_name.Map.t
    -> Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t
    -> Build_path_prefix_map.map String.Map.t

  val build_and_save_maps : Super_context.t Context_name.Map.t -> unit Memo.t
end
