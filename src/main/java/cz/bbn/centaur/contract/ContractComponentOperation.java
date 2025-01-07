package cz.bbn.cerberus.contract;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.ContactPersonService;
import cz.bbn.cerberus.contactperson.dto.ContactPersonObjectTypeEnum;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.contract.dto.ContractFilterDto;
import cz.bbn.cerberus.contract.dto.ContractInternalType;
import cz.bbn.cerberus.contract.ui.ContractBackOfficeDetailView;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.contract.ui.component.ContractBackOfficeFilterDtoComponent;
import cz.bbn.cerberus.contract.ui.component.ContractNewDialog;
import cz.bbn.cerberus.contract.ui.component.ContractSalesFilterDtoComponent;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.custompermission.CustomPermissionService;
import cz.bbn.cerberus.custompermission.dto.CustomUserPermissionDto;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.custompermission.ui.CustomPermissionSingleListener;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.opportunity.dto.OpportunityDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Component
@Slf4j
public class ContractComponentOperation {

    private final ContractService contractService;
    private final CustomPermissionService customPermissionService;
    private final AppEnv appEnv;
    private final ContactPersonService contactPersonService;
    private final UserService userService;
    private final ListService listService;

    public ContractComponentOperation(ContractService contractService, CustomPermissionService customPermissionService,
                                      AppEnv appEnv, ContactPersonService contactPersonService, UserService userService,
                                      ListService listService) {
        this.contractService = contractService;
        this.customPermissionService = customPermissionService;
        this.appEnv = appEnv;
        this.contactPersonService = contactPersonService;
        this.userService = userService;
        this.listService = listService;
    }

    public SaveAction<ContractDto> getSaveAction(CustomPermissionSingleListener listener,
                                                 ContractNewDialog contractNewDialog,
                                                 ComboBox<ContractDto> connectedContract) {
        return (newDto, originalDto) -> {
            if (newDto.getType() != null && Boolean.TRUE.equals(newDto.getType().getConnectionRequired())
                    && newDto.getConnectedContract() == null) {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
                connectedContract.setInvalid(true);
            } else {
                try {
                    save(newDto, originalDto, listener, contractNewDialog);
                } catch (SystemException ex) {
                    log.error(TextValues.SYSTEM_EXCEPTION, ex);
                    ErrorNotification.show(ex, appEnv);
                }
            }
        };
    }

    public List<String> getContactPersonsToAdd(ContractDto dto) {
        return contactPersonService.findNotUsedByObjectByUser(ContactPersonObjectTypeEnum.CONTRACT, dto.getId());
    }

    public List<ContractDto> getContractListBySubject(String subject) {
        return contractService.getContractListBySubject(subject);
    }

    public ItemsAction<ContractDto> getItemsAction(ContractSalesFilterDtoComponent filterComponent) {
        return (query, orderList) -> {
            ContractFilterDto filter = filterComponent.getContractFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return contractService.findContractDtoPage(filter);
        };
    }

    public ItemsAction<ContractDto> getItemsAction(ContractBackOfficeFilterDtoComponent filterComponent) {
        return (query, orderList) -> {
            ContractFilterDto filter = filterComponent.getContractFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return contractService.findContractDtoPage(filter);
        };
    }

    public ItemsAction<ContractDto> getItemsAction(ContractFilterDto filter) {
        return (query, orderList) -> {
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return contractService.findContractDtoPage(filter);
        };
    }

    public List<ContractDto> findContractAllowedList() {
        return contractService.getMyInvoiceEditContractList();
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                contractService.deleteContract(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public ItemsAction<ContractDto> getConnectedContractItemAction(String id, ContractInternalType internalType) {
        return (query, orderList) -> {
            ContractFilterDto filter = new ContractFilterDto();
            filter.setParentContractId(id);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            filter.setContractInternalType(internalType);
            return contractService.findContractDtoPage(filter);
        };
    }

    public List<ContractDto> getMyInvoiceEditContractList() {
        return contractService.getMyInvoiceEditContractList();
    }

    public ComponentEventListener<ClickEvent<Button>> getNewContractDialogEvent(
            AppInfiniteGrid<ContractDto> grid, SubjectDto subjectDto, ContractInternalType contractInternalType,
            AppDialog dialog, OpportunityDto opportunityDto) {
        return buttonClickEvent -> {
            dialog.close();
            ContractNewDialog contractNewDialog = new ContractNewDialog(grid, this,
                    userService, subjectDto, findAllowedTypes(contractInternalType),
                    appEnv, listService, contractInternalType, opportunityDto);
            contractNewDialog.open();
        };
    }

    public ComponentEventListener<ClickEvent<? extends com.vaadin.flow.component.Component>> getNewContractDialogEvent(
            AppInfiniteGrid<ContractDto> grid, ContractInternalType contractInternalType) {
        return buttonClickEvent -> {
            ContractNewDialog contractNewDialog = new ContractNewDialog(grid, this,
                    userService, null, findAllowedTypes(contractInternalType),
                    appEnv, listService, contractInternalType, null);
            contractNewDialog.open();
        };
    }


    public List<SubjectDto> getOurCompanyList() {
        List<SubjectDto> allSubjectList = listService.getSubjectDtoList();
        List<SubjectDto> ourCompanyList = new ArrayList<>();
        Set<String> subjectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        for (SubjectDto subjectDto : allSubjectList) {
            if (Boolean.TRUE.equals(subjectDto.getOwnCompany()) && Boolean.FALSE.equals(subjectDto.getDeleted())
                    && subjectIdSet.contains(subjectDto.getId())) {
                ourCompanyList.add(subjectDto);
            }
        }
        return ourCompanyList;
    }

    public List<SubjectDto> getOurCompCustomerList() {
        List<SubjectDto> allSubjectList = listService.getSubjectDtoList();
        List<SubjectDto> ourCompanyList = new ArrayList<>();
        Set<String> subjectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        for (SubjectDto subjectDto : allSubjectList) {
            if ((Boolean.TRUE.equals(subjectDto.getOwnCompany()) || Boolean.TRUE.equals(subjectDto.getCustomer()))
                    && Boolean.FALSE.equals(subjectDto.getDeleted()) && subjectIdSet.contains(subjectDto.getId())) {
                ourCompanyList.add(subjectDto);
            }
        }

        return ourCompanyList;
    }

    public SubjectDto getOurCompanyValue(List<SubjectDto> ourCompanyList) {
        return ourCompanyList.stream()
                .filter(subjectDto -> subjectDto.getId().equals(appEnv.getDefaultCompanySubject()))
                .findAny().orElse(null);
    }

    public List<SubjectDto> getSupplierList() {
        List<SubjectDto> allSubjectList = listService.getSubjectDtoList();
        List<SubjectDto> ourCompanyList = new ArrayList<>();
        Set<String> subjectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        for (SubjectDto subjectDto : allSubjectList) {
            if (Boolean.TRUE.equals(subjectDto.getSupplier()) && Boolean.FALSE.equals(subjectDto.getDeleted())
                    && subjectIdSet.contains(subjectDto.getId())) {
                ourCompanyList.add(subjectDto);
            }
        }
        return ourCompanyList;
    }

    public List<SubjectDto> getSupplierOwnCompList() {
        List<SubjectDto> allSubjectList = listService.getSubjectDtoList();
        List<SubjectDto> ourCompanyList = new ArrayList<>();
        Set<String> subjectIdSet = SecurityUtils.getCustomReadPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
        for (SubjectDto subjectDto : allSubjectList) {
            if ((Boolean.TRUE.equals(subjectDto.getSupplier()) || Boolean.TRUE.equals(subjectDto.getOwnCompany()))
                    && Boolean.FALSE.equals(subjectDto.getDeleted()) && subjectIdSet.contains(subjectDto.getId())) {
                ourCompanyList.add(subjectDto);
            }
        }
        return ourCompanyList;
    }

    private void save(ContractDto newDto, ContractDto originalDto,
                      CustomPermissionSingleListener listener, ContractNewDialog contractNewDialog)
            throws SystemException {
        if (originalDto.getId() != null) {
            contractService.updateContract(newDto, originalDto);
            processCustomPerms(listener, newDto);
            UI.getCurrent().getPage().getHistory().back();
        } else {
            contractService.saveContract(newDto);
            contractNewDialog.showWarning(false);
            contractNewDialog.close();
            if (newDto.getInternalType() == ContractInternalType.OPERATIONAL) {
                UI.getCurrent().navigate(ContractBackOfficeDetailView.ROUTE.concat("/").concat(newDto.getId()));
            } else {
                UI.getCurrent().navigate(ContractSalesDetailView.ROUTE.concat("/").concat(newDto.getId()));
            }
        }
        SuccessNotification.showSavingSuccess(appEnv);
    }

    private void processCustomPerms(CustomPermissionSingleListener listener, ContractDto newDto) {
        Set<PermUserDto> userSet = listener.getSinglePermissionUserList();
        if (userSet != null) {
            Set<CustomUserPermissionDto> tempSet = new HashSet<>();
            for (PermUserDto user : userSet) {
                if (!user.isPermanent()) {
                    CustomUserPermissionDto customUserPermissionDto = new CustomUserPermissionDto(
                            DomainEnum.CONTRACT_DOMAIN_NAME.getValue(), Permission.CONTRACT_VIEW.name(),
                            user.getId(), newDto.getId(), true
                    );
                    tempSet.add(customUserPermissionDto);
                }
            }
            customPermissionService.saveByUser(tempSet, DomainEnum.CONTRACT_DOMAIN_NAME.getValue(),
                    newDto.getId(), Permission.CONTRACT_VIEW.name());
        }
    }

    public List<ContractTypeDto> findAllowedTypes(ContractInternalType internalType) {
        List<ContractTypeDto> typeList = new ArrayList<>();
        for (ContractTypeDto typeDto : listService.getContractTypeDtoList()) {
            if (Boolean.TRUE.equals(typeDto.getAllowed()) && (isRightType(typeDto, internalType))) {
                typeList.add(typeDto);
            }
        }
        return typeList;
    }

    private boolean isRightType(ContractTypeDto typeDto, ContractInternalType interType) {
        return (isSales(typeDto, interType) || isOperational(typeDto, interType) || isSupplier(typeDto, interType));
    }

    private boolean isSales(ContractTypeDto typeDto, ContractInternalType internalType) {
        return internalType == ContractInternalType.SALES && Boolean.TRUE.equals(typeDto.getSales());
    }

    private boolean isOperational(ContractTypeDto typeDto, ContractInternalType internalType) {
        return internalType == ContractInternalType.OPERATIONAL && Boolean.TRUE.equals(typeDto.getOperational());
    }

    private boolean isSupplier(ContractTypeDto typeDto, ContractInternalType internalType) {
        return internalType == ContractInternalType.SUPPLIER && Boolean.TRUE.equals(typeDto.getSupplierCo());
    }
}
