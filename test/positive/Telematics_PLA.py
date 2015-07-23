from jsir.IR import *

c0_comp = Abstract("c0_comp");
c1_options = Abstract("c1_options");
c0_ECU = Abstract("c0_ECU").extending(c0_comp);
c0_display = Abstract("c0_display").extending(c0_comp);
c0_plaECU = Abstract("c0_plaECU").extending(c0_ECU);
c0_telematicsSystem = Clafer("c0_telematicsSystem").withCard(1, 1);
c0_channel = c0_telematicsSystem.addChild("c0_channel").withCard(1, 1).withGroupCard(1, 1);
c0_single = c0_channel.addChild("c0_single").withCard(0, 1);
c0_dual = c0_channel.addChild("c0_dual").withCard(0, 1);
c0_extraDisplay = c0_telematicsSystem.addChild("c0_extraDisplay").withCard(0, 1);
c0_size = c0_telematicsSystem.addChild("c0_size").withCard(1, 1).withGroupCard(1, 1);
c0_small = c0_size.addChild("c0_small").withCard(0, 1);
c0_large = c0_size.addChild("c0_large").withCard(0, 1);
c0_version = c0_comp.addChild("c0_version").withCard(1, 1);
c0_server = c0_display.addChild("c0_server").withCard(1, 1);
c0_options = c0_display.addChild("c0_options").withCard(1, 1).extending(c1_options);
c1_size = c1_options.addChild("c1_size").withCard(1, 1).withGroupCard(1, 1);
c1_small = c1_size.addChild("c1_small").withCard(0, 1);
c1_large = c1_size.addChild("c1_large").withCard(0, 1);
c0_cache = c1_options.addChild("c0_cache").withCard(0, 1);
c2_size = c0_cache.addChild("c2_size").withCard(1, 1);
c0_fixed = c2_size.addChild("c0_fixed").withCard(0, 1);
c1_display = c0_plaECU.addChild("c1_display").withCard(1, 2).extending(c0_display);
c0_ECU1 = Clafer("c0_ECU1").withCard(1, 1).extending(c0_plaECU);
c0_ECU2 = Clafer("c0_ECU2").withCard(0, 1).extending(c0_plaECU);
c0_master = c0_ECU2.addChild("c0_master").withCard(1, 1);
c0_version.refTo("int");
c0_server.refTo(c0_ECU);
c2_size.refTo("int");
c0_master.refTo(c0_ECU1);
Constraint(some(join(join(glob(c0_telematicsSystem), c0_channel), c0_dual)));
Constraint(some(join(glob(c0_telematicsSystem), c0_extraDisplay)));
Constraint(some(join(join(glob(c0_telematicsSystem), c0_size), c0_large)));
Constraint(equal(joinRef(join(glob(c0_comp), c0_version)), constant(1)));
c0_telematicsSystem.addConstraint(ifOnlyIf(some(join(join($this(), c0_channel), c0_dual)), some(glob(c0_ECU2))));
c0_telematicsSystem.addConstraint(ifOnlyIf(some(join($this(), c0_extraDisplay)), equal(card(join(glob(c0_ECU1), c1_display)), constant(2))));
c0_telematicsSystem.addConstraint(ifOnlyIf(some(join($this(), c0_extraDisplay)), implies(some(glob(c0_ECU2)), equal(card(join(glob(c0_ECU2), c1_display)), constant(2)))));
c0_telematicsSystem.addConstraint(ifOnlyIf(some(join(join($this(), c0_size), c0_large)), none(join(join(join(join(glob(c0_plaECU), c1_display), c0_options), c1_size), c1_small))));
c0_telematicsSystem.addConstraint(ifOnlyIf(some(join(join($this(), c0_size), c0_small)), none(join(join(join(join(glob(c0_plaECU), c1_display), c0_options), c1_size), c1_large))));
c0_display.addConstraint(greaterThanEqual(joinRef(join($this(), c0_version)), joinRef(join(joinRef(join($this(), c0_server)), c0_version))));
c1_options.addConstraint(implies(and(some(join(join($this(), c1_size), c1_small)), some(join($this(), c0_cache))), some(join(join(join($this(), c0_cache), c2_size), c0_fixed))));
c1_display.addConstraint(equal(joinRef(join($this(), c0_server)), joinParent($this())));
c1_display.addConstraint(none(join(join($this(), c0_options), c0_cache)));
scope({c0_ECU:2, c0_cache:4, c0_comp:6, c0_display:4, c0_fixed:4, c0_options:4, c0_plaECU:2, c0_server:4, c0_version:6, c1_display:4, c1_large:4, c1_options:4, c1_size:4, c1_small:4, c2_size:4});
defaultScope(1);
stringLength(16);
