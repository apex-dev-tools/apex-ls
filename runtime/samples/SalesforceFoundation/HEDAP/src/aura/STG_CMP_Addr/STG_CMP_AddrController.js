({
    toggleIsView : function(component, event) {
		component.set("v.isView", event.getParam("isView"));
	},
	runBackfill : function (component, event, helper) {
		helper.runBackfill(component);
	}
})