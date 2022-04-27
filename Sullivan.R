
# This output was derived in Algebra_HLE.R
# it gives the same expectancies, but with NO MOVING PARTS
# 

pix <- c(0.942832933525023, 0.925366777629796, 0.913878052953308, 0.905779576715131, 
         0.899819586082213, 0.895228598060479, 0.891415053920396, 0.887843431788832, 
         0.883977734168754, 0.879243670749217, 0.872988101030959, 0.864422353013589, 
         0.85253805474243, 0.836000848599391, 0.813416659727759, 0.783638907018477, 
         0.745769572698891, 0.699353730294645, 0.64468449536391, 0.583095062238541, 
         0.5170619612345, 0.449955803009552, 0.385423852181739, 0.326631969263549, 
         0.275728924251826, 0.23377510423577, 0.201092734009695, 0.17782145726523, 
         0.164539328404487, 0.163055237515445)

lx <- c(2, 1.98744304104249, 1.97166480704484, 1.95251099982402, 1.92979047491913, 
  1.90327639864472, 1.87269654546685, 1.83771283501313, 1.7978930617532, 
  1.75267423877303, 1.70131782289022, 1.64285829147167, 1.57605111088635, 
  1.49933944580084, 1.41094952851575, 1.30916546330524, 1.19275620234035, 
  1.06156436022103, 0.917173279730504, 0.763469971998791, 0.606832911763207, 
  0.455676937888572, 0.319219834302956, 0.205583276597247, 0.119696424658899, 
  0.0618521812086196, 0.0278148278482655, 0.0106670648655719, 0.00341876365030296, 
  0.000898335418648003)

a <- seq(50,108,by=2)
# survival curve
plot(a, lx, type = 'l', las = 2,
     main = "commonly drawn picture, please don't\ninterpret as literal depiction of the life course!")
# color part of it healthy
polygon(c(a, 108, 50), c(lx * pix, 0,0), col = "#76a0e3")
# color part of it unhealthy
polygon(c(a, rev(a)), c(lx * pix, rev(lx)), col = "#b57412")

(HLE <- sum(pix * lx))        # same HLE
(ULE <- sum((1 - pix) * lx))  # same ULE
dput(c(HLE, ULE))
c(28.2758869643443, 5.61608147350015)
# copied from Alegebra_HLE.R, exact
c(28.2758869643443, 5.61608147350015)
# copied from (age 50) ID_HLE.R, exact
c(28.2758869643443, 5.61608147350015)


# really there's nothing more to this
# You can do this for strata and find gradients
# You can do trends this way, etc,
# and the general approach is widely applicable
# just please be aware of the limitations.
# prevalence is a lagging indicator, and it 
# has cohort history in it. It is *not* the result
# of year t's incidence. But if it seems to move 
# in regular ways then why not exploit these?



