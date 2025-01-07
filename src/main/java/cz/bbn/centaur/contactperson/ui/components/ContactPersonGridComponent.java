package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.dialog.WarningListDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactperson.ui.ContactPersonDetailView;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class ContactPersonGridComponent extends AppInfiniteGrid<ContactPersonDto> {

    private final ContactPersonComponentOperation componentOperation;
    private final DeleteAction deleteAction;
    private final ListService listService;

    public ContactPersonGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                      ItemsAction<ContactPersonDto> itemsAction,
                                      ContactPersonComponentOperation componentOperation,
                                      ListService listService) {
        super(deleteAction, appEnv, itemsAction);
        this.componentOperation = componentOperation;
        this.deleteAction = deleteAction;
        this.listService = listService;
        initGrid();
    }

    public ContactPersonGridComponent(ItemsAction<ContactPersonDto> itemsAction, AppEnv appEnv) {
        super(appEnv, itemsAction);
        this.componentOperation = null;
        this.deleteAction = null;
        this.listService = null;
        initGrid();
    }

    private void initGrid() {
        addColumn(ContactPersonDto::getFirstName).setHeader(Transl.get("First name"))
                .setSortable(true).setKey("firstName");
        addColumn(ContactPersonDto::getLastName).setHeader(Transl.get("Last name"))
                .setSortable(true).setKey("lastName");
        addColumn(ContactPersonDto::getEmail).setHeader(Transl.get("Email")).setSortable(true).setKey("email");
        addColumn(contactPersonDto -> AppUtils.phoneFormat(contactPersonDto.getPhonePrefixDto(),
                contactPersonDto.getPhone())).setHeader(Transl.get("Phone"))
                .setSortable(true).setKey("phone");
        addColumn(contactPersonDto -> {
            if (contactPersonDto.getContactPersonType() != null) {
                return contactPersonDto.getContactPersonType().getName();
            }
            return "";
        })
                .setHeader(Transl.get("Contact type"))
                .setSortable(true)
                .setKey("contactPersonType");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        if (componentOperation != null) {
            addColumn(new ComponentRenderer<>(this::getGridButtons))
                    .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                    .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                    .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

            addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
        } else {
            this.allowMultiselect();
        }

    }

    private HorizontalLayout getGridButtons(ContactPersonDto clickedItem) {
        HorizontalLayout layout = new HorizontalLayout();
        layout.setClassName("buttons-layout");

        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_DELETE)
                && !Boolean.TRUE.equals(clickedItem.getDeleted())) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, clickedItem.getId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> projectList = getProjectList(clickedItem);
                List<String> subjectList = getSubjectList(clickedItem);
                List<String> contractList = getContractList(clickedItem);
                List<String> opportunityList = getOpportunityList(clickedItem);
                if (projectList.isEmpty() && subjectList.isEmpty() && contractList.isEmpty()
                        && opportunityList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(
                                    this, clickedItem.getId(),
                                    Transl.get("Are you sure you want to delete contact person {0} ",
                                            clickedItem.getName()), getAppEnv(), true);
                    deleteConfirmDialog.open();
                } else {
                    WarningListDialog warningListDialog =
                            new WarningListDialog(projectList, subjectList, contractList, opportunityList,
                                    Transl.get("Contact person cannot be deleted"),
                                    Transl.get("Contact person is assigned to"),
                                    Transl.get("Projects").concat(":"), Transl.get("Subjects").concat(":"),
                                    Transl.get("Contracts").concat(":"), Transl.get("Opportunity").concat(":"),
                                    deleteAction, clickedItem.getId());
                    warningListDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete contact person"));
            layout.add(delete);
        }

        return layout;
    }

    private List<String> getProjectList(ContactPersonDto clickedItem) {
        List<String> projectList = new ArrayList<>();
        List<String> idList = componentOperation.getProjectList(clickedItem.getId());
        Map<String, ProjectDto> projectMap = listService.getProjectDtoMap();
        for (String id : idList) {
            if (projectMap.containsKey(id)) {
                ProjectDto projectDto = projectMap.get(id);
                projectList.add(projectDto.getName().concat(" (").concat(projectDto.getId()).concat(")"));
            }
        }
        return projectList;
    }

    private List<String> getSubjectList(ContactPersonDto clickedItem) {
        List<String> subjectList = new ArrayList<>();
        List<String> idList = componentOperation.getSubjectList(clickedItem.getId());
        Map<String, SubjectDto> subjectMap = listService.getSubjectMap();
        for (String id : idList) {
            if (subjectMap.containsKey(id)) {
                SubjectDto subjectDto = subjectMap.get(id);
                subjectList.add(subjectDto.getName().concat(" (").concat(subjectDto.getId()).concat(")"));
            }
        }
        return subjectList;
    }

    private List<String> getContractList(ContactPersonDto clickedItem) {
        List<String> contractList = new ArrayList<>();
        List<String> idList = componentOperation.getContractList(clickedItem.getId());
        Map<String, ContractDto> contractMap = listService.getContractMap();
        for (String id : idList) {
            if (contractMap.containsKey(id)) {
                ContractDto contractDto = contractMap.get(id);
                contractList.add(contractDto.getName().concat(" (").concat(contractDto.getId()).concat(")"));
            }
        }
        return contractList;
    }

    private List<String> getOpportunityList(ContactPersonDto clickedItem) {
        List<String> opportunityList = new ArrayList<>();
        List<String> idList = componentOperation.getOpportunityList(clickedItem.getId());
        Map<String, OpportunityDto> opportunityMap = listService.getOpportunityMap();
        for (String id : idList) {
            if (opportunityMap.containsKey(id)) {
                OpportunityDto opportunityDto = opportunityMap.get(id);
                opportunityList.add(opportunityDto.getName().concat(" (").concat(opportunityDto.getId()).concat(")"));
            }
        }
        return opportunityList;
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(ContactPersonDetailView.ROUTE + "/" + code);
    }
}
