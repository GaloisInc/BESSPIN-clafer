open util/integer
pred show {}


fact { #c0_Boolean = 0 }
abstract sig c0_Boolean
{}

fact { #c0_Integer = 0 }
abstract sig c0_Integer
{}

fact { #c0_String = 0 }
abstract sig c0_String
{}

fact { #c0_UnlimitedNatural = 0 }
abstract sig c0_UnlimitedNatural
{}

fact { #c0_Sequence = 0 }
abstract sig c0_Sequence
{}

fact { #c0_Set = 0 }
abstract sig c0_Set
{}

fact { #c0_Element = 0 }
abstract sig c0_Element
{ r_c0_ownedElement : set c0_ownedElement
, r_c0_owner : lone c0_owner }
{ all disj x, y : this.@r_c0_ownedElement | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_ownedElement | this in (x.(@ref.(@r_c0_owner.@ref)))
  all  x : this.@r_c0_owner | this in (x.(@ref.(@r_c0_ownedElement.@ref))) }

sig c0_ownedElement
{ ref : one c0_Element }
{ one @r_c0_ownedElement.this }

sig c0_owner
{ ref : one c0_Element }
{ one @r_c0_owner.this }

fact { #c0_NamedElement = 0 }
abstract sig c0_NamedElement extends c0_Element
{ r_c0_name : lone c0_name
, r_c0_qualifiedName : lone c0_qualifiedName
, r_c0_visibility : lone c0_visibility }

sig c0_name extends c0_String
{}
{ one @r_c0_name.this }

sig c0_qualifiedName extends c0_String
{}
{ one @r_c0_qualifiedName.this }

sig c0_visibility
{ ref : one c0_VisibilityKind }
{ one @r_c0_visibility.this }

fact { #c0_ValueSpecification = 0 }
abstract sig c0_ValueSpecification extends c0_NamedElement
{}

fact { #c0_DirectedRelationship = 0 }
abstract sig c0_DirectedRelationship
{ r_c0_relatedElement : some c0_relatedElement
, r_c0_source : some c0_source
, r_c0_target : some c0_target }
{ all disj x, y : this.@r_c0_relatedElement | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_source | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_target | (x.@ref) != (y.@ref)  }

sig c0_relatedElement
{ ref : one c0_Element }
{ one @r_c0_relatedElement.this }

sig c0_source
{ ref : one c0_Element }
{ one @r_c0_source.this }

sig c0_target
{ ref : one c0_Element }
{ one @r_c0_target.this }

fact { #c0_Class = 0 }
abstract sig c0_Class extends c0_Classifier
{ r_c0_isActive : lone c0_isActive
, r_c0_superClass : set c0_superClass
, r_c0_nestedClassifier : set c0_nestedClassifier }
{ all disj x, y : this.@r_c0_superClass | (x.@ref) != (y.@ref)  }

sig c0_isActive
{}
{ one @r_c0_isActive.this }

sig c0_superClass
{ ref : one c0_Class }
{ one @r_c0_superClass.this }

sig c0_nestedClassifier extends c0_Classifier
{}
{ one @r_c0_nestedClassifier.this }

fact { #c0_Property = 0 }
abstract sig c0_Property extends c0_StructuralFeature
{ r_c0_default : lone c0_default
, r_c0_isComposite : lone c0_isComposite
, r_c0_isDerived : lone c0_isDerived
, r_c0_isDerivedUnion : lone c0_isDerivedUnion
, r_c0_aggregation : lone c0_aggregation
, r_c0_class : lone c0_class
, r_c0_opposite : lone c0_opposite
, r_c0_owningAssociation : lone c0_owningAssociation
, r_c0_redefinedProperty : set c0_redefinedProperty
, r_c0_subsettedProperty : set c0_subsettedProperty
, r_c0_datatype : lone c0_datatype
, r_c0_association : lone c0_association
, r_c0_defaultValue : lone c0_defaultValue
, r_c0_qualifier : set c0_qualifier
, r_c0_associationEnd : lone c0_associationEnd }
{ all  x : this.@r_c0_owningAssociation | this in (x.(@ref.@r_c0_ownedEnd))
  all disj x, y : this.@r_c0_redefinedProperty | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_subsettedProperty | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_datatype | this in (x.(@ref.@r_c0_ownedAttribute))
  all  x : this.@r_c0_association | this in (x.(@ref.(@r_c0_memberEnd.@ref)))
  all  x : this.@r_c0_associationEnd | this in (x.(@ref.@r_c0_qualifier)) }

sig c0_default extends c0_String
{}
{ one @r_c0_default.this }

sig c0_isComposite
{}
{ one @r_c0_isComposite.this }

sig c0_isDerived
{}
{ one @r_c0_isDerived.this }

sig c0_isDerivedUnion
{}
{ one @r_c0_isDerivedUnion.this }

sig c0_aggregation
{ ref : one c0_AggregationKind }
{ one @r_c0_aggregation.this }

sig c0_class
{ ref : one c0_Class }
{ one @r_c0_class.this }

sig c0_opposite
{ ref : one c0_Property }
{ one @r_c0_opposite.this }

sig c0_owningAssociation
{ ref : one c0_Association }
{ one @r_c0_owningAssociation.this }

sig c0_redefinedProperty
{ ref : one c0_Property }
{ one @r_c0_redefinedProperty.this }

sig c0_subsettedProperty
{ ref : one c0_Property }
{ one @r_c0_subsettedProperty.this }

sig c0_datatype
{ ref : one c0_DataType }
{ one @r_c0_datatype.this }

sig c0_association
{ ref : one c0_Association }
{ one @r_c0_association.this }

sig c0_defaultValue extends c0_ValueSpecification
{}
{ one @r_c0_defaultValue.this }

sig c0_qualifier extends c0_Property
{}
{ one @r_c0_qualifier.this
  (this.~@r_c0_qualifier) in (this.(@r_c0_associationEnd.@ref)) }

sig c0_associationEnd
{ ref : one c0_Property }
{ one @r_c0_associationEnd.this }

fact { #c0_Enumeration = 0 }
abstract sig c0_Enumeration extends c0_DataType
{ r_c0_ownedLiteral : set c0_ownedLiteral }

sig c0_ownedLiteral extends c0_EnumerationLiteral
{}
{ one @r_c0_ownedLiteral.this
  (this.~@r_c0_ownedLiteral) in (this.(@r_c0_enumeration.@ref)) }

fact { #c0_DataType = 0 }
abstract sig c0_DataType extends c0_Classifier
{ r_c0_ownedAttribute : set c0_ownedAttribute }

sig c0_ownedAttribute extends c0_Property
{}
{ one @r_c0_ownedAttribute.this
  (this.~@r_c0_ownedAttribute) in (this.(@r_c0_datatype.@ref)) }

fact { #c0_EnumerationLiteral = 0 }
abstract sig c0_EnumerationLiteral extends c0_InstanceSpecification
{ r_c0_enumeration : lone c0_enumeration }
{ all  x : this.@r_c0_enumeration | this in (x.(@ref.@r_c0_ownedLiteral)) }

sig c0_enumeration
{ ref : one c0_Enumeration }
{ one @r_c0_enumeration.this }

fact { #c0_PrimitiveType = 0 }
abstract sig c0_PrimitiveType extends c0_DataType
{}

fact { #c0_Classifier = 0 }
abstract sig c0_Classifier extends c0_NamedElement
{ r_c0_isAbstract : lone c0_isAbstract
, r_c0_feature : set c0_feature
, r_c0_inheritedMember : set c0_inheritedMember
, r_c0_general : set c0_general
, r_c0_generalization : set c0_generalization
, r_c0_attribute : set c0_attribute
, r_c0_redefinedClassifier : set c0_redefinedClassifier
, r_c0_powertypeExtent : set c0_powertypeExtent }
{ all disj x, y : this.@r_c0_feature | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_feature | this in (x.(@ref.(@r_c0_featuringClassifier.@ref)))
  all disj x, y : this.@r_c0_inheritedMember | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_general | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_attribute | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_redefinedClassifier | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_powertypeExtent | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_powertypeExtent | this in (x.(@ref.(@r_c0_powertype.@ref))) }

sig c0_isAbstract
{}
{ one @r_c0_isAbstract.this }

sig c0_feature
{ ref : one c0_Feature }
{ one @r_c0_feature.this }

sig c0_inheritedMember
{ ref : one c0_NamedElement }
{ one @r_c0_inheritedMember.this }

sig c0_general
{ ref : one c0_Classifier }
{ one @r_c0_general.this }

sig c0_generalization extends c0_Generalization
{}
{ one @r_c0_generalization.this
  (this.~@r_c0_generalization) in (this.(@r_c0_specific.@ref)) }

sig c0_attribute
{ ref : one c0_Property }
{ one @r_c0_attribute.this }

sig c0_redefinedClassifier
{ ref : one c0_Classifier }
{ one @r_c0_redefinedClassifier.this }

sig c0_powertypeExtent
{ ref : one c0_GeneralizationSet }
{ one @r_c0_powertypeExtent.this }

fact { #c0_Feature = 0 }
abstract sig c0_Feature extends c0_NamedElement
{ r_c0_isStatic : lone c0_isStatic
, r_c0_featuringClassifier : set c0_featuringClassifier }
{ all disj x, y : this.@r_c0_featuringClassifier | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_featuringClassifier | this in (x.(@ref.(@r_c0_feature.@ref))) }

sig c0_isStatic
{}
{ one @r_c0_isStatic.this }

sig c0_featuringClassifier
{ ref : one c0_Classifier }
{ one @r_c0_featuringClassifier.this }

abstract sig c0_VisibilityKind
{}

one sig c0_package extends c0_VisibilityKind
{}

one sig c0_private extends c0_VisibilityKind
{}

one sig c0_protected extends c0_VisibilityKind
{}

one sig c0_public extends c0_VisibilityKind
{}

fact { #c0_LiteralBoolean = 0 }
abstract sig c0_LiteralBoolean extends c0_LiteralSpecification
{ r_c0_value : lone c0_value }

sig c0_value
{}
{ one @r_c0_value.this }

fact { #c0_LiteralSpecification = 0 }
abstract sig c0_LiteralSpecification extends c0_ValueSpecification
{}

fact { #c0_LiteralString = 0 }
abstract sig c0_LiteralString extends c0_LiteralSpecification
{ r_c1_value : lone c1_value }

sig c1_value extends c0_String
{}
{ one @r_c1_value.this }

fact { #c0_LiteralNull = 0 }
abstract sig c0_LiteralNull extends c0_LiteralSpecification
{}

fact { #c0_LiteralInteger = 0 }
abstract sig c0_LiteralInteger extends c0_LiteralSpecification
{ r_c2_value : lone c2_value }

sig c2_value
{ ref : one c0_Integer }
{ one @r_c2_value.this }

fact { #c0_LiteralUnlimitedNatural = 0 }
abstract sig c0_LiteralUnlimitedNatural extends c0_LiteralSpecification
{ r_c3_value : lone c3_value }

sig c3_value extends c0_UnlimitedNatural
{}
{ one @r_c3_value.this }

fact { #c0_StructuralFeature = 0 }
abstract sig c0_StructuralFeature extends c0_Feature
{ r_c0_isReadOnly : lone c0_isReadOnly
, r_c0_isOrdered : lone c0_isOrdered
, r_c0_isUnique : lone c0_isUnique
, r_c0_lower : lone c0_lower
, r_c0_upper : lone c0_upper
, r_c0_upperValue : lone c0_upperValue
, r_c0_lowerValue : lone c0_lowerValue }

sig c0_isReadOnly
{}
{ one @r_c0_isReadOnly.this }

sig c0_isOrdered
{}
{ one @r_c0_isOrdered.this }

sig c0_isUnique
{}
{ one @r_c0_isUnique.this }

sig c0_lower
{ ref : one c0_Integer }
{ one @r_c0_lower.this }

sig c0_upper extends c0_UnlimitedNatural
{}
{ one @r_c0_upper.this }

sig c0_upperValue extends c0_ValueSpecification
{}
{ one @r_c0_upperValue.this }

sig c0_lowerValue extends c0_ValueSpecification
{}
{ one @r_c0_lowerValue.this }

fact { #c0_InstanceSpecification = 0 }
abstract sig c0_InstanceSpecification extends c0_NamedElement
{ r_c0_slot : set c0_slot
, r_c0_classifier : set c0_classifier
, r_c0_specification : lone c0_specification }
{ all disj x, y : this.@r_c0_classifier | (x.@ref) != (y.@ref)  }

sig c0_slot extends c0_Slot
{}
{ one @r_c0_slot.this
  (this.~@r_c0_slot) in (this.(@r_c0_owningInstance.@ref)) }

sig c0_classifier
{ ref : one c0_Classifier }
{ one @r_c0_classifier.this }

sig c0_specification extends c0_ValueSpecification
{}
{ one @r_c0_specification.this }

fact { #c0_Slot = 0 }
abstract sig c0_Slot extends c0_Element
{ r_c0_owningInstance : one c0_owningInstance
, r_c4_value : set c4_value
, r_c0_definingFeature : one c0_definingFeature }
{ all  x : this.@r_c0_owningInstance | this in (x.(@ref.@r_c0_slot)) }

sig c0_owningInstance
{ ref : one c0_InstanceSpecification }
{ one @r_c0_owningInstance.this }

sig c4_value extends c0_ValueSpecification
{}
{ one @r_c4_value.this }

sig c0_definingFeature
{ ref : one c0_StructuralFeature }
{ one @r_c0_definingFeature.this }

fact { #c0_InstanceValue = 0 }
abstract sig c0_InstanceValue extends c0_ValueSpecification
{ r_c0_instance : one c0_instance }

sig c0_instance
{ ref : one c0_InstanceSpecification }
{ one @r_c0_instance.this }

fact { #c0_Generalization = 0 }
abstract sig c0_Generalization extends c0_DirectedRelationship
{ r_c0_isSubstitutable : lone c0_isSubstitutable
, r_c0_specific : one c0_specific
, r_c1_general : one c1_general
, r_c0_generalizationSet : set c0_generalizationSet }
{ all  x : this.@r_c0_specific | this in (x.(@ref.@r_c0_generalization))
  all disj x, y : this.@r_c0_generalizationSet | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_generalizationSet | this in (x.(@ref.(@r_c1_generalization.@ref))) }

sig c0_isSubstitutable
{}
{ one @r_c0_isSubstitutable.this }

sig c0_specific
{ ref : one c0_Classifier }
{ one @r_c0_specific.this }

sig c1_general
{ ref : one c0_Classifier }
{ one @r_c1_general.this }

sig c0_generalizationSet
{ ref : one c0_GeneralizationSet }
{ one @r_c0_generalizationSet.this }

fact { #c0_Association = 0 }
abstract sig c0_Association extends c0_Classifier
{ r_c1_relatedElement : some c1_relatedElement
, r_c1_isDerived : lone c1_isDerived
, r_c0_ownedEnd : set c0_ownedEnd
, r_c0_endType : some c0_endType
, r_c0_memberEnd : set c0_memberEnd }
{ all disj x, y : this.@r_c1_relatedElement | (x.@ref) != (y.@ref) 
  all disj x, y : this.@r_c0_endType | (x.@ref) != (y.@ref) 
  2 <= #r_c0_memberEnd
  all disj x, y : this.@r_c0_memberEnd | (x.@ref) != (y.@ref) 
  all  x : this.@r_c0_memberEnd | this in (x.(@ref.(@r_c0_association.@ref)))
  ((#(this.@r_c0_memberEnd)) > 2) => ((this.(@r_c0_memberEnd.@ref)) in (this.@r_c0_ownedEnd)) }

sig c1_relatedElement
{ ref : one c0_Element }
{ one @r_c1_relatedElement.this }

sig c1_isDerived
{}
{ one @r_c1_isDerived.this }

sig c0_ownedEnd extends c0_Property
{}
{ one @r_c0_ownedEnd.this
  (this.~@r_c0_ownedEnd) in (this.(@r_c0_owningAssociation.@ref)) }

sig c0_endType
{ ref : one c0_NamedElement }
{ one @r_c0_endType.this }

sig c0_memberEnd
{ ref : one c0_Property }
{ one @r_c0_memberEnd.this }

fact { #c0_ExtensionEnd = 0 }
abstract sig c0_ExtensionEnd extends c0_Property
{}

abstract sig c0_AggregationKind
{}

one sig c0_composite extends c0_AggregationKind
{}

one sig c0_none extends c0_AggregationKind
{}

one sig c0_shared extends c0_AggregationKind
{}

fact { #c0_GeneralizationSet = 0 }
abstract sig c0_GeneralizationSet extends c0_NamedElement
{ r_c0_isCovering : lone c0_isCovering
, r_c0_isDisjoint : lone c0_isDisjoint
, r_c0_powertype : lone c0_powertype
, r_c1_generalization : set c1_generalization }
{ all  x : this.@r_c0_powertype | this in (x.(@ref.(@r_c0_powertypeExtent.@ref)))
  all disj x, y : this.@r_c1_generalization | (x.@ref) != (y.@ref) 
  all  x : this.@r_c1_generalization | this in (x.(@ref.(@r_c0_generalizationSet.@ref))) }

sig c0_isCovering
{}
{ one @r_c0_isCovering.this }

sig c0_isDisjoint
{}
{ one @r_c0_isDisjoint.this }

sig c0_powertype
{ ref : one c0_Classifier }
{ one @r_c0_powertype.this }

sig c1_generalization
{ ref : one c0_Generalization }
{ one @r_c1_generalization.this }

