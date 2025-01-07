package cz.bbn.cerberus.contract.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.permission.Permission;

import java.util.List;

public class ContractTabsComponent extends TabsComponent<TabSimpleComponent> {

    public ContractTabsComponent(String title, List<TabEntry> tabEntryList, Button createProject, Button addDocument,
                                 Button linkContactPerson, Button linkSupplier, Button addAreaTechnologySign,
                                 boolean hasContractEdit, EntityNewComponentOperation entityNewComponentOperation,
                                 ContractDto dto) {
        super(title, tabEntryList, entityNewComponentOperation);

        if (SecurityUtils.hasPermission(Permission.CONTRACT_EDIT) && hasContractEdit) {
            if (SecurityUtils.hasPermission(Permission.PROJECT_EDIT)) {
                addToButtonFooter(createProject);
            }
            if (SecurityUtils.hasPermission(Permission.DOCUMENT_EDIT)) {
                addToButtonFooter(addDocument);
            }
            if (SecurityUtils.hasPermission(Permission.CONTRACT_AREA_TECHNOLOGY_SIGN_EDIT)) {
                addToButtonFooter(addAreaTechnologySign);
            }
            if (SecurityUtils.hasCustomPermission(DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), dto.getId(),
                    Permission.CONTRACT_LINK_CONTACT_PERSON.name())) {
                addToButtonFooter(linkContactPerson);
            }
            if (SecurityUtils.hasPermission(Permission.SUPPLIER_EDIT)) {
                addToButtonFooter(linkSupplier);
            }
        }

        addBackButton();
        if (hasContractEdit && SecurityUtils.hasPermission(Permission.CONTRACT_EDIT)) {
            addSaveButton();
        }
    }
}
