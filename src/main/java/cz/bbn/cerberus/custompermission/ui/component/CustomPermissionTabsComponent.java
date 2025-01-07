package cz.bbn.cerberus.custompermission.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionViewListener;

import java.util.List;

public class CustomPermissionTabsComponent extends TabsComponent<TabSimpleComponent> {


    public CustomPermissionTabsComponent(CustomPermissionViewListener listener, String title, List<TabEntry> tabList,
                                         EntityNewComponentOperation entityNewComponentOperation) {
        super(title, tabList, entityNewComponentOperation);
        getContent().setHeightFull();
        setHeightFull();

        Button saveButton = VaadinComponents.getSubmitButton();
        saveButton.addClickListener(event -> listener.saveAll());
        addSaveButton(saveButton);
    }

}
