package cz.bbn.cerberus.commons.component.ui.slidetab;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.TypeByObject;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

public class NewEntityButtonsComponent extends FormLayout {

    private final Builder builder;

    private final List<Button> buttonList = new ArrayList<>();

    public NewEntityButtonsComponent(EntityNewComponentOperation entityNewComponentOperation) {
        builder = new Builder(entityNewComponentOperation, EntityNewType.ALL);
        initComponent();
    }

    public NewEntityButtonsComponent(EntityNewComponentOperation entityNewComponentOperation, EntityNewType type) {
        builder = new Builder(entityNewComponentOperation, type);
        initComponent();
    }

    private NewEntityButtonsComponent(Builder builder) {
        this.builder = builder;
        initComponent();
    }

    private void initComponent() {
        getElement().getStyle().set("padding", "10px");
        switch (builder.type) {
            case ALL:
                buttonList.addAll(getNewButtonSalesList());
                buttonList.addAll(getNewButtonBackofficeList());
                break;
            case SALES:
                buttonList.addAll(getNewButtonSalesList());
                break;
            case BACKOFFICE:
                buttonList.addAll(getNewButtonBackofficeList());
                break;
        }
        for (Button button : buttonList) {
            button.getElement().getStyle().set("background-color", "white");
            this.add(button);
        }
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public int getButtonCount() {
        return buttonList.size();
    }

    private List<Button> getNewButtonSalesList() {
        List<Button> buttonListActual = new ArrayList<>();
        if (SecurityUtils.hasPermission(Permission.SUBJECT_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New subject"));
            button.addClickListener(e ->
                    builder.entityNewComponentOperation.getNewSubjectEvent(null).onComponentEvent(e));
            buttonListActual.add(button);
        }
        
        if (SecurityUtils.hasPermission(Permission.OPPORTUNITY_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New opportunity"));
            SubjectDto actualSubjectDto =
                    builder.subjectDto != null && builder.opportunityFillSubjectDto ? builder.subjectDto : null;
            button.addClickListener(e -> builder.entityNewComponentOperation.getNewOpportunityEvent(null,
                    actualSubjectDto).onComponentEvent(e));
            buttonListActual.add(button);
        }

        if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New contact person"));
            button.addClickListener(e -> builder.entityNewComponentOperation.getNewContactPersonDialogEvent(null,
                    builder.typeByObjectList).onComponentEvent(e));
            buttonListActual.add(button);
        }

        if (SecurityUtils.hasPermission(Permission.PROJECT_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New project"));
            SubjectDto actualSubjectDto =
                    builder.subjectDto != null && builder.projectFillSubjectDto
                            && Boolean.FALSE.equals(builder.subjectDto.getOwnCompany())
                            ? builder.subjectDto : null;
            button.addClickListener(e -> builder.entityNewComponentOperation.getNewProjectEvent(null, actualSubjectDto,
                    builder.contractDto).onComponentEvent(e));
            buttonListActual.add(button);
        }

        if (SecurityUtils.hasPermission(Permission.CONTRACT_EDIT)
                && SecurityUtils.hasPermission(Permission.SALES_CONTRACT_VIEW)) {
            SubjectDto actualSubjectDto =
                    builder.subjectDto != null && builder.contractFillSubjectDto ? builder.subjectDto : null;
            OpportunityDto actualOpportunity = builder.opportunityDto != null ? builder.opportunityDto : null;
            Button button = VaadinComponents.getNewButton(Transl.get("New contract"));
            button.addClickListener(e -> builder.entityNewComponentOperation.getNewContractDialogEvent(
                    null, actualSubjectDto, actualOpportunity).onComponentEvent(e));
            buttonListActual.add(button);
        }
        if (SecurityUtils.hasPermission(Permission.OFFER_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New offer"));
            SubjectDto actualSubjectDto =
                    builder.subjectDto != null && builder.offerFillSubjectDto ? builder.subjectDto : null;
            button.addClickListener(e -> builder.entityNewComponentOperation.getNewOfferEvent(
                    null, actualSubjectDto, builder.opportunityDto).onComponentEvent(e));
            buttonListActual.add(button);
        }

        if (SecurityUtils.hasPermission(Permission.CONTRACT_EDIT)
                && SecurityUtils.hasPermission(Permission.INVOICE_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New invoice"));
            button.addClickListener(e -> builder.entityNewComponentOperation.getInvoiceEvent(
                    builder.invoiceGrid, builder.contractDto, builder.subjectDto).onComponentEvent(e));
            buttonListActual.add(button);
        }
        return buttonListActual;
    }

    private List<Button> getNewButtonBackofficeList() {
        List<Button> buttonList = new ArrayList<>();
        if (SecurityUtils.hasPermission(Permission.ASSET_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New asset"));
            button.addClickListener(e ->
                    builder.entityNewComponentOperation.getNewAssetEvent(null, builder.subjectDto).onComponentEvent(e));
            buttonList.add(button);
        }
        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_EDIT)) {
            Button button = VaadinComponents.getNewButton(Transl.get("New employee"));
            button.addClickListener(e ->
                    builder.entityNewComponentOperation.getNewEmployeeDialogEvent(null).onComponentEvent(e));
            buttonList.add(button);
        }
        return buttonList;
    }

    public static boolean hasAllPermission() {
        return SecurityUtils.hasPermission(Permission.OPPORTUNITY_EDIT) ||
                SecurityUtils.hasPermission(Permission.CONTACT_PERSON_EDIT) ||
                SecurityUtils.hasPermission(Permission.PROJECT_EDIT) ||
                SecurityUtils.hasPermission(Permission.CONTRACT_EDIT) ||
                SecurityUtils.hasPermission(Permission.DOCUMENT_EDIT) ||
                SecurityUtils.hasPermission(Permission.ASSET_EDIT) ||
                SecurityUtils.hasPermission(Permission.EMPLOYEE_EDIT);
    }

    @Getter
    public static class Builder {

        private final EntityNewComponentOperation entityNewComponentOperation;
        private final EntityNewType type;
        private SubjectDto subjectDto;
        private ContractDto contractDto;
        private OpportunityDto opportunityDto;
        private boolean opportunityFillSubjectDto = true;
        private boolean contractFillSubjectDto = true;
        private boolean offerFillSubjectDto = true;
        private boolean projectFillSubjectDto = true;
        private List<TypeByObject> typeByObjectList;
        private AppInfiniteGrid<InvoiceDto> invoiceGrid;

        public Builder(EntityNewComponentOperation entityNewComponentOperation, EntityNewType type) {
            this.entityNewComponentOperation = entityNewComponentOperation;
            this.type = type;
        }

        public Builder setSubjectDto(SubjectDto subjectDto) {
            this.subjectDto = subjectDto;
            return this;
        }

        public Builder setOpportunityFillSubjectDto(boolean opportunityFillSubjectDto) {
            this.opportunityFillSubjectDto = opportunityFillSubjectDto;
            return this;
        }

        public Builder setContractFillSubjectDto(boolean contractFillSubjectDto) {
            this.contractFillSubjectDto = contractFillSubjectDto;
            return this;
        }

        public Builder setTypeByObjectList(List<TypeByObject> typeByObjectList) {
            this.typeByObjectList = typeByObjectList;
            return this;
        }

        public Builder setOfferFillSubjectDto(boolean offerFillSubjectDto) {
            this.offerFillSubjectDto = offerFillSubjectDto;
            return this;
        }

        public Builder setProjectFillSubjectDto(boolean projectFillSubjectDto) {
            this.projectFillSubjectDto = projectFillSubjectDto;
            return this;
        }

        public Builder setContractDto(ContractDto contractDto) {
            this.contractDto = contractDto;
            return this;
        }

        public Builder setInvoiceGrid(AppInfiniteGrid<InvoiceDto> grid) {
            this.invoiceGrid = grid;
            return this;
        }

        public Builder setOpportunityDto(OpportunityDto opportunityDto) {
            this.opportunityDto = opportunityDto;
            return this;
        }

        public NewEntityButtonsComponent build() {
            return new NewEntityButtonsComponent(this);
        }
    }
}
