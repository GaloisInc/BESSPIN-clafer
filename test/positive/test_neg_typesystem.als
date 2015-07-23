open util/integer
pred show {}


abstract sig c0_Measurable
{ r_c0_performance : one c0_performance }

sig c0_performance
{ ref : one Int }
{ one @r_c0_performance.this }

one sig c0_D extends c0_Measurable
{}

one sig c0_C extends c0_Measurable
{}

fact { (c0_C.(@r_c0_performance.@ref)) = (-1.mul[(c0_D.(@r_c0_performance.@ref))]) }
