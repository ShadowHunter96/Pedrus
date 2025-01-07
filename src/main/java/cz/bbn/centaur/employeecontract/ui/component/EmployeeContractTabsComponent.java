package cz.bbn.cerberus.employeecontract.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class EmployeeContractTabsComponent extends TabsComponent<TabSimpleComponent> {

    public EmployeeContractTabsComponent(String title, List<TabEntry> tabEntryList,
                                         EntityNewComponentOperation entityNewComponentOperation,
                                         EmployeeContractComponentOperation componentOperation,
                                         EmployeeContractDto originalDto) {
        super(title, tabEntryList, entityNewComponentOperation);

        addBackButton();
        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_ARCHIVE) && !originalDto.getArchived()) {
            Button archive = VaadinComponents.getButton(Transl.get("Archive"), VaadinIcon.ARCHIVE.create());
            archive.addClickListener(e ->
                componentOperation.archive(originalDto, true, EmployeeContractView.ROUTE)
            );
            addToButtonFooter(archive);
        } else if(SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_UNARCHIVE) && originalDto.getArchived()){
            Button unarchive = VaadinComponents.getButton(Transl.get("Unarchive"), VaadinIcon.ARROW_FORWARD.create());
            unarchive.addClickListener(e ->
                componentOperation.archive(originalDto, false, EmployeeContractView.ROUTE)
            );
            addToButtonFooter(unarchive);
        }

        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_CONTRACT_DELETE)) {
            Button delete = VaadinComponents.getDeleteButton();
            delete.setText(Transl.get("Delete"));
            delete.addClickListener(e -> componentOperation.getDeleteConfirmDialog(originalDto));
            addToButtonFooter(delete);
        }

        Button linkContract = VaadinComponents.getLinkButton(Transl.get("Link contract"));
        linkContract.addClickListener(e -> componentOperation.getLinkContractDialog(originalDto));

        Button createAddendum = VaadinComponents.getButton(Transl.get("Create addendum"), VaadinIcon.FILE_ADD.create());
        createAddendum.addClickListener(e -> componentOperation.createAddendum(originalDto));

        if (SecurityUtils.hasPermission(Permission.CONTRACT_EDIT)) {
            addToButtonFooter(linkContract, createAddendum);
            addSaveButton();
        }
    }
}
