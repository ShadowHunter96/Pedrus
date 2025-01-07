package cz.bbn.cerberus.asset.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class AssetTabsComponent extends TabsComponent<TabSimpleComponent> {

    public AssetTabsComponent(String title, List<TabEntry> tabEntryList, boolean showSubmitButton,
                              Button linkAsset, Button addDocumentButton,
                              EntityNewComponentOperation entityNewComponentOperation) {
        super(title, tabEntryList, entityNewComponentOperation);
        if (SecurityUtils.hasPermission(Permission.ASSET_EDIT)) {
            addToButtonFooter(linkAsset);
        }
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_EDIT)
                && SecurityUtils.hasPermission(Permission.ASSET_DOCUMENT_UPLOAD)) {
            addToButtonFooter(addDocumentButton);
        }
        addBackButton();
        if (showSubmitButton) {
            addSaveButton();
        }
    }
}
