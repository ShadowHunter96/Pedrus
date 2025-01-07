package cz.bbn.cerberus.employeecontract;

import com.vaadin.flow.component.ClickEvent;
import com.vaadin.flow.component.ComponentEventListener;
import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.contracttype.dto.ContractTypeDto;
import cz.bbn.cerberus.employee.EmployeeService;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractFilterDto;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractDetailView;
import cz.bbn.cerberus.employeecontract.ui.EmployeeContractView;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractConfirmDeleteDialog;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractFilterDtoComponent;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractLinkContractDialog;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractNewDialog;
import cz.bbn.cerberus.enumeration.EnumerationService;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Component
@Slf4j
public class EmployeeContractComponentOperation {

    private final EmployeeContractService employeeContractService;
    private final EmployeeService employeeService;
    private final EnumerationService enumerationService;
    private final ListService listService;
    private final AppEnv appEnv;

    public EmployeeContractComponentOperation(EmployeeContractService employeeContractService,
                                              EmployeeService employeeService, EnumerationService enumerationService,
                                              ListService listService, AppEnv appEnv) {
        this.employeeContractService = employeeContractService;
        this.employeeService = employeeService;
        this.enumerationService = enumerationService;
        this.listService = listService;
        this.appEnv = appEnv;
    }

    public ItemsAction<EmployeeContractDto> getItemsAction(EmployeeContractFilterDtoComponent filterComponent) {
        return (query, orderList) -> {
            EmployeeContractFilterDto filter = filterComponent.getEmployeeContractFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return employeeContractService.findContractDtoPage(filter);
        };
    }

    public List<EmployeeDto> getEmployeeList() {
        return employeeService.findValidEmployeeList();
    }

    public List<ContractTypeDto> getTypeList() {
        List<ContractTypeDto> allTypeList = listService.getContractTypeDtoList();
        List<ContractTypeDto> employeeCoList = new ArrayList<>();
        for (ContractTypeDto contractType : allTypeList) {
            if (Boolean.TRUE.equals(contractType.getEmployeeCo()) && Boolean.TRUE.equals(contractType.getAllowed())) {
                employeeCoList.add(contractType);
            }
        }
        return employeeCoList;
    }

    public EmployeeContractDto getEmployeeContract(String id) throws SystemException {
        return employeeContractService.getEmployeeContract(id);
    }

    public SaveAction<EmployeeContractDto> getSaveAction(EmployeeContractNewDialog dialog) {
        return (newDto, originalDto) -> {
            try {
                if (newDto.getId() != null && !newDto.getId().isEmpty()) {
                    employeeContractService.updateEmployeeContract(newDto, originalDto);
                    UI.getCurrent().navigate(EmployeeContractView.ROUTE);
                } else {
                    newDto.setArchived(false);
                    newDto.setDeleted(false);
                    newDto.setCreationDate(LocalDateTime.now());
                    String id = employeeContractService.saveEmployeeContract(newDto);
                    UI.getCurrent().navigate(EmployeeContractDetailView.ROUTE.concat("/").concat(id));
                }
                if (dialog != null) {
                    dialog.showWarning(false);
                    dialog.close();
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public List<EnumerationDto> getStateList() {
        return enumerationService.getEnumerationDtoListByTypeNotDeletedAllowed("EMP_CONTRACT_STATE");
    }

    public ComponentEventListener<
            ClickEvent<? extends com.vaadin.flow.component.Component>
            > getEmployeeContractNewEvent(AppInfiniteGrid appInfiniteGrid) {
        return e -> {
            EmployeeContractNewDialog dialog = new EmployeeContractNewDialog(appInfiniteGrid, this, appEnv);
            dialog.open();
        };
    }

    public SubjectDto getOwnCompany() {
        return listService.getSubjectDtoListByOwnCompany().stream()
                .filter(actualSubjectDto -> actualSubjectDto.getId().equals(appEnv.getDefaultCompanySubject()))
                .findAny().orElse(null);
    }

    public List<SubjectDto> getOwnCompanyList() {
        return listService.getSubjectDtoListByOwnCompany();
    }

    public List<EmployeeContractDto> getEmpConListExceptId(String id) {
        List<EmployeeContractDto> list = employeeContractService.getEmployeeContractListExceptId(id);
        return list;
    }

    public void archive(EmployeeContractDto dto, boolean archive, String route) {
        EmployeeContractDto originalDto = SerializationUtils.clone(dto);
        if (dto.getId() != null) {
            dto.setArchived(archive);
            try {
                employeeContractService.updateEmployeeContract(dto, originalDto);
                UI.getCurrent().navigate(route);
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    public void getDeleteConfirmDialog(EmployeeContractDto dto) {
        EmployeeContractConfirmDeleteDialog dialog = new EmployeeContractConfirmDeleteDialog(dto, this);
        dialog.open();
    }

    public void deleteEmpContract(EmployeeContractDto dto, AppDialog dialog) {
        EmployeeContractDto originalDto = SerializationUtils.clone(dto);
        if (dto.getId() != null) {
            dto.setDeleted(true);
            try {
                employeeContractService.updateEmployeeContract(dto, originalDto);
                if (dialog != null) {
                    dialog.showWarning(false);
                    dialog.close();
                }
                UI.getCurrent().navigate(EmployeeContractView.ROUTE);
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    public void linkContract(EmployeeContractDto dto, EmployeeContractDto linkWith, AppDialog dialog) {
        EmployeeContractDto originalDto = SerializationUtils.clone(dto);
        if (dto.getId() != null) {
            dto.setLinkedContractId(linkWith.getId());
            try {
                employeeContractService.updateEmployeeContract(dto, originalDto);
                if (dialog != null) {
                    dialog.showWarning(false);
                    dialog.close();
                }
                UI.getCurrent().navigate(EmployeeContractView.ROUTE);
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    public void createAddendum(EmployeeContractDto originalDto) {
        EmployeeContractDto dto = new EmployeeContractDto();
        dto.setLinkedContractId(originalDto.getId());
        EmployeeContractNewDialog newDialog = new EmployeeContractNewDialog(dto, this, appEnv);
        newDialog.open();
    }

    public void getLinkContractDialog(EmployeeContractDto originalDto) {
        EmployeeContractLinkContractDialog dialog =
                new EmployeeContractLinkContractDialog(getEmpConListExceptId(originalDto.getId()), this, originalDto);
        dialog.open();
    }

    public ItemsAction<EmployeeContractDto> getLinkedEmpConItemActions(EmployeeContractDto dto) {
        return (query, orderList) -> {
            EmployeeContractFilterDto filter = new EmployeeContractFilterDto();
            filter.setLinkedContract(dto.getId());
            filter.setShowArchived(false);
            filter.setShowDeleted(false);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return employeeContractService.findContractDtoPage(filter);
        };
    }
}
