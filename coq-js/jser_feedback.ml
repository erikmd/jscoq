(* Coq JavaScript API. Based in the coq source code and js_of_ocaml.
 *
 * By Emilio J. Gallego Arias, Mines ParisTech, Paris.
 * LICENSE: GPLv3+
 *
 * We provide a message-based asynchronous API for communication with
 * Coq. Our object listens to the following messages:
 *
 * And emits:
 *
 * - CoqLogEvent(level, msg): Log [msg] of priority [level].
 *
 *)

type 'a gxml =
  [%import: 'a Xml_datatype.gxml
  ]
  [@@deriving yojson]

type xml =
  [%import: Xml_datatype.xml
  [@with
     Xml_datatype.gxml := gxml;
  ]]
  [@@deriving yojson]

type richpp = Richpp.richpp

let richpp_of_yojson sexp =
  let open Result in
  match xml_of_yojson sexp with
  | Ok xml  -> Ok (Richpp.richpp_of_xml xml)
  | Error s -> Error s

let richpp_to_yojson rpp  = xml_to_yojson (Richpp.repr rpp)

type loc =
  [%import: Loc.t]
  [@@deriving yojson]
  [@@warning -39]

type stateid  = Stateid.t

type _stateid = int
  [@@deriving yojson]
  [@@warning -39]

let stateid_of_yojson json =
  let open Result in
  match _stateid_of_yojson json with
  | Ok id   -> Ok (Stateid.of_int id)
  | Error s -> Error s

let stateid_to_yojson sid  = _stateid_to_yojson (Stateid.to_int sid)

type level =
  [%import: Feedback.level]
  [@@deriving yojson]
  [@@warning -39]

type edit_id =
  [%import: Feedback.edit_id]
  [@@deriving yojson]

type route_id =
  [%import: Feedback.route_id]
  [@@deriving yojson]

type edit_or_state_id =
  [%import: Feedback.edit_or_state_id
  [@with
     Feedback.edit_id := edit_id;
     state_id := stateid;
  ]]
  [@@deriving yojson]

type feedback_content =
  [%import: Feedback.feedback_content
  [@with
     Stateid.t := stateid;
     Loc.t := loc;
     Xml_datatype.xml := xml;
     Richpp.richpp    := richpp;
  ]]
  [@@deriving yojson]

type feedback =
  [%import: Feedback.feedback
  [@with
     Stateid.t := stateid;
     Feedback.route_id := route_id;
  ]]
  [@@deriving yojson]
