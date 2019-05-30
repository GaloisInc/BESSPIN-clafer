# BESSPIN `.fm.json` format

The FMJSON backend (`clafer -m fmjson`) converts a Clafer model to a JSON
feature model representation for use with other BESSPIN tools.  The FMJSON
format directly encodes the underlying model, with none of Clafer's abstraction
features, which should make it easier for tools to work with.

Not all Clafer models can be translated to FMJSON format.  The FMJSON backend
will report an error if it encounters an unsupported construct.


## Semantic model

An FMJSON feature model consists of a forest (a set of trees) of features and a
set of constraints.

Features are identified by name, with names being unique within the file.  They
have the properties [described below](#features), in addition to their parent
and child relationships within a tree.  There are no cycles or sharing: each
feature is the child of exactly one feature or else is the root of a tree.

Constraints are boolean expressions over features.  They can reference any
feature (by its unique name), regardless of its position in the forest.

A "configuration" is an assignment of boolean values (true/false, meaning
enabled/disabled) to each feature in the feature model.  The feature model
classifies configurations as valid or invalid, based on the structure of the
forest, the properties of individual features, and the results of evaluating
the constraint expressions under the configuration.


## Top level

The top-level JSON object of an FMJSON file has the following fields:

* `features`: An object that maps the name of each feature to [its
  definition](#features).  Every feature in the forest appears in this map,
  with no extras - every feature in `features` is accessible by traversing
  `roots` and `children` fields of individual features.

* `roots`: An array containing the names of the root feature of each tree.

* `constraints`: An array of [constraint expressions](#constraints).

* `version`: An object containing file format [version
  information](#versioning).


## Features

A feature object contains the following fields:

* `name`: The name of this feature.  This is also the feature's key in the
  top-level `features` map.

* `parent`: The name of this feature's parent in the tree, or `null` if it is
  the root of a tree.

* `children`: An array containing the names of the children of this feature.

* `card`: The cardinality of this feature, represented as a string.  Legal
  values are:

    - `"on"`: The feature is required to be enabled in all valid configurations
      of this feature model.  (Corresponds to Clafer cardinality `1..1`.)
    - `"off"`: The feature is required to be disabled in all valid
      configurations of this feature model.  (Clafer cardinality `0..0`.)
    - `"opt"`: The feature is allowed to be either enabled or disabled.
      (Clafer cardinality `0..1`.)

  Arbitrary numeric intervals, as used in Clafer, are not supported.

* `gcard`: The group cardinality for children of this feature, represented as a
  string.  Legal values are:

    - `"opt"`: Any number of child features can be enabled if this feature is
      enabled.  (Corresponds to Clafer group cardinality `0..*`, the default.)
    - `"or"`: One or more child features must be enabled if this feature is
      enabled.  (Clafer group cardinality `1..*`.)
    - `"mux"`: Zero or one of the child features must be enabled if this
      feature is enabled.  (Clafer group cardinality `0..1`.)
    - `"xor"`: Exactly one child feature must be enabled if this feature is
      enabled.  (Clafer group cardinality `1..1`.)

  Arbitrary numeric intervals, as used in Clafer, are not supported.

The features of a feature model form a collection of trees.  Use the `parent`
and `children` fields and the top-level `roots` array to traverse the trees.


## Constraints

A constraint expression is a JSON object with a `kind` field, and additional
field depending on the kind.

### `op`

When `kind` is `"op"`, the object has two additional fields:

* `op`: A string representing the name of the operation.  Supported values are:

    - `"and"`: Boolean and.  Takes any number of arguments.
    - `"or"`: Boolean or.  Takes any number of arguments.
    - `"xor"`: Boolean exclusive or.  Takes any number of arguments.
    - `"not"`: Boolean negation.  Takes one argument.
    - `"imp"`: Implication.  `A imp B` is equivalent to `(not A) or B`.  Takes
      two arguments.
    - `"eqv"`: Equivalence.  `A eqv B` is equivalent to `(A and B) or ((not A)
      and (not B))`.  Takes two arguments.

* `args`: An array of constraint expression objects.  The length of the array
  should match the requirements of the chosen `op`.

### `lit`

When `kind` is `"lit"`, the object has one additional field:

* `val`: A JSON boolean, `true` or `false`.

### `feat`

When `kind` is `"feat"`, the object has one additional field:

* `name`: The name of a feature.


## Versioning

The version object has one mandatory key:

* `base`: An integer, indicating the version of the base FMJSON format used for
  the current file.

The current version of the FMJSON format is **1**.  Future revisions of the
FMJSON format will be assigned higher version numbers.

Tools that output FMJSON must set `version.base` to an FMJSON format version
that supports all constructs used in the output file.  There may be more than
one such version, especially if a tool generates only a subset of supported
constructs.  Tools should not rely on the contents of the model when choosing
an FMJSON version (for example, using a lower version for simple models, and
switching to a higher version only for models that use additional features),
since this makes it harder to check for compatibility between different tools.

Tools that input FMJSON should check `version.base` against a whitelist of
known supported versions.  There are no backwards compatibility guarantees
between versions.  Checking against a whitelist ensures that incompatibilities
between tools are detected early, rather than failing only on specific models
or appearing to succeed but assigning different semantics to the file.

### Extensions

Tools can define extensions to the FMJSON format.  This is mainly intended to
allow annotating features, constraints, or the entire model with additional
information.  For example, some tools may wish to track whether a constraint
was provided by user action or by an automated process of some kind.

Presence of an extension is indicated by including an additional field to the
version object whose name is the name of the extension and whose value is an
integer version number for this extension.

Extensions can add new extension-specific fields to any object (including the
root object), and the can restrict the range of values permitted in any
existing field.  They cannot expand the range of allowed values.  This
requirement ensures that tools can safely ignore unknown extensions.

Tools that transform or otherwise manipulate FMJSON files should not attempt to
preserve data associated with unknown extensions.  Extensions may impose
arbitrary invariants, which cannot be preserved in general without knowledge of
the extensions in question.
