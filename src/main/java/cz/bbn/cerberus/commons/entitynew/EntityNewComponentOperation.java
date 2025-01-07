package cz.bbn.cerberus.commons.entitynew;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.ui.component.AssetNewDialog;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.config.SpringContext;
import cz.bbn.cerberus.contactperson.ContactPersonComponentOperation;
import cz.bbn.cerberus.contactperson.dto.TypeByObject;
import cz.bbn.cerberus.contactperson.ui.components.ContactPersonNewDialog;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.ui.component.ContractPickTypeDialog;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.ui.component.EmployeeNewDialog;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.ui.component.AddInvoicingDialog;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.offer.OfferComponentOpperation;
import cz.bbn.cerberus.offer.ui.component.OfferNewDialog;
import cz.bbn.cerberus.opportunity.OpportunityComponentOperation;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.opportunity.dto.OpportunitySimpleDto;
import cz.bbn.cerberus.opportunity.ui.component.OpportunityNewDialog;
import cz.bbn.cerberus.phoneprefix.PhonePrefixService;
import cz.bbn.cerberus.project.ProjectComponentOperation;
import cz.bbn.cerberus.project.ui.component.ProjectNewDialog;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.SubjectService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.component.SubjectPickTypeDialog;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.user.UserService;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class EntityNewComponentOperation {

    private final SubjectComponentOperation subjectComponentOperation;
    private final OpportunityComponentOperation opportunityComponentOperation;
    private final ContactPersonComponentOperation contactPersonComponentOperation;
    private final ProjectComponentOperation projectComponentOperation;
    private final ContractComponentOperation contractComponentOperation;
    private final OfferComponentOpperation offerComponentOpperation;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final AssetComponentOperation assetComponentOperation;
    private final EmployeeComponentOperation employeeComponentOperation;
    private final TaskComponentOperation taskComponentOperation;
    private final ListService listService;
    private final AppEnv appEnv;
    private final PhonePrefixService phonePrefixService;
    private final UserService userService;
    private final SubjectService subjectService;
    private final NoteComponentOperation noteComponentOperation;

    public EntityNewComponentOperation(SubjectComponentOperation subjectComponentOperation,
                                       OpportunityComponentOperation opportunityComponentOperation,
                                       ContactPersonComponentOperation contactPersonComponentOperation,
                                       ProjectComponentOperation projectComponentOperation,
                                       ContractComponentOperation contractComponentOperation,
                                       OfferComponentOpperation offerComponentOpperation,
                                       InvoiceComponentOperation invoiceComponentOperation,
                                       AssetComponentOperation assetComponentOperation,
                                       EmployeeComponentOperation employeeComponentOperation,
                                       TaskComponentOperation taskComponentOperation,
                                       ListService listService, AppEnv appEnv, PhonePrefixService phonePrefixService,
                                       UserService userService, SubjectService subjectService,
                                       NoteComponentOperation noteComponentOperation) {
        this.subjectComponentOperation = subjectComponentOperation;
        this.opportunityComponentOperation = opportunityComponentOperation;
        this.contactPersonComponentOperation = contactPersonComponentOperation;
        this.projectComponentOperation = projectComponentOperation;
        this.contractComponentOperation = contractComponentOperation;
        this.offerComponentOpperation = offerComponentOpperation;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.assetComponentOperation = assetComponentOperation;
        this.employeeComponentOperation = employeeComponentOperation;
        this.taskComponentOperation = taskComponentOperation;
        this.listService = listService;
        this.appEnv = appEnv;
        this.phonePrefixService = phonePrefixService;
        this.userService = userService;
        this.subjectService = subjectService;
        this.noteComponentOperation = noteComponentOperation;
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>>
    getNewSubjectEvent(AppInfiniteGrid<SubjectDto> grid) {
        return buttonClickEvent -> {
            SubjectPickTypeDialog subjectPickTypeDialog =
                    new SubjectPickTypeDialog(grid, subjectComponentOperation, appEnv, listService);
            subjectPickTypeDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getNewOpportunityEvent(
            AppInfiniteGrid<OpportunitySimpleDto> grid, SubjectDto subjectDto) {
        return buttonClickEvent -> {
            OpportunityNewDialog opportunityNewDialog = new OpportunityNewDialog(grid, appEnv,
                    opportunityComponentOperation, subjectDto, listService);
            opportunityNewDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>>
    getNewContactPersonDialogEvent(
            AppInfiniteGrid<?> grid, List<TypeByObject> typeList) {
        return buttonClickEvent -> {
            ContactPersonNewDialog newOpportunityDialog = new ContactPersonNewDialog(grid, appEnv,
                    contactPersonComponentOperation, phonePrefixService, typeList);
            newOpportunityDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getNewProjectEvent(
            AppInfiniteGrid<?> grid, SubjectDto subjectDto, ContractDto contractDto) {
        return buttonClickEvent -> {
            ProjectNewDialog projectNewDialog = new ProjectNewDialog(grid, projectComponentOperation, appEnv,
                    subjectDto, contractDto, listService);
            projectNewDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getNewOfferEvent(
            AppInfiniteGrid<?> grid, SubjectDto dto, OpportunityDto opportunityDto) {
        return buttonClickEvent -> {
            OfferNewDialog offerNewDialog = new OfferNewDialog(grid, offerComponentOpperation, dto,
                    userService, listService, appEnv, opportunityDto, subjectService);
            offerNewDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getNewContractDialogEvent(
            AppInfiniteGrid<ContractDto> grid, SubjectDto subjectDto, OpportunityDto opportunityDto) {
        return buttonClickEvent -> {
            ContractPickTypeDialog pickDialog =
                    new ContractPickTypeDialog(grid, contractComponentOperation, subjectDto, opportunityDto);
            pickDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getInvoiceEvent(AppInfiniteGrid<?> grid, ContractDto contractDto,
                                                                                                             SubjectDto subjectDto) {
        return buttonClickEvent -> {
            // protoze se trida EntityNewComponentOperation pouziva na kazdem view, nechci vsude tuto tridu doplnovat. Proto ji volam ze staticContextu
            AreaTechnologyComponentOperation areaTechnologyComponentOperation =
                    SpringContext.getBean(AreaTechnologyComponentOperation.class);
            AddInvoicingDialog offerNewDialog = new AddInvoicingDialog(contractDto, subjectDto,
                    invoiceComponentOperation, subjectComponentOperation, contractComponentOperation, grid, appEnv,
                    areaTechnologyComponentOperation);
            offerNewDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>>
    getNewEmployeeDialogEvent(AppInfiniteGrid<EmployeeDto> grid) {
        return buttonClickEvent -> {
            EmployeeNewDialog employeeNewDialog = new EmployeeNewDialog(
                    grid, employeeComponentOperation, appEnv, listService);
            employeeNewDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>>
    getNewAssetEvent(AppInfiniteGrid<AssetSimpleDto> grid, SubjectDto subjectDto) {
        return buttonClickEvent -> {
            AssetNewDialog supplierNewDialog = new AssetNewDialog(grid, assetComponentOperation, appEnv, listService,
                    subjectDto);
            supplierNewDialog.open();
        };
    }

    public TaskComponentOperation getEventComponentOperation() {
        return taskComponentOperation;
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }

    public ListService getListService() {
        return listService;
    }

    public NoteComponentOperation getNoteComponentService() {
        return noteComponentOperation;
    }

    public int getNoteCount(NoteFilterDto filter) {
        return noteComponentOperation.getNoteCountByType(filter);
    }
}
