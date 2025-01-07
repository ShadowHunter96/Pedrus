package cz.bbn.cerberus.employee;

import com.vaadin.flow.component.UI;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectDto;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectFilterDto;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.dto.EmployeeFilterDto;
import cz.bbn.cerberus.employee.dto.EmployeeLinkDto;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeByObjectId;
import cz.bbn.cerberus.employee.ui.EmployeeDetailView;
import cz.bbn.cerberus.employee.ui.EmployeeView;
import cz.bbn.cerberus.employee.ui.component.EmployeeByObjectFilterComponent;
import cz.bbn.cerberus.employee.ui.component.EmployeeFilterComponent;
import cz.bbn.cerberus.employee.ui.component.EmployeeNewDialog;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;

@Component
@Slf4j
public class EmployeeComponentOperation {

    private final EmployeeService employeeService;
    private final UserService userService;
    private final AppEnv appEnv;

    public EmployeeComponentOperation(EmployeeService employeeService, UserService userService, AppEnv appEnv) {
        this.employeeService = employeeService;
        this.userService = userService;
        this.appEnv = appEnv;
    }

    public ItemsAction<EmployeeDto> getEmployeeDtoItemsAction(EmployeeFilterComponent filterComponent) {
        return (query, orderList) -> {
            EmployeeFilterDto filter = filterComponent.getEmployeeFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return employeeService.findEmployeeDtoPage(filter);
        };
    }

    public ItemsAction<EmployeeByObjectDto> getEmployeeByObjectDtoItemsAction(
            EmployeeByObjectFilterComponent filterComponent, String objectId, ObjectType objectType) {
        return (query, orderList) -> {
            EmployeeByObjectFilterDto filter = filterComponent.getEmployeeByObjectFilterDto();
            filter.setObjectId(objectId);
            filter.setObjectType(objectType);
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return employeeService.findEmployeeByObjectDtoPage(filter);
        };
    }

    public DeleteAction getEmployeeByObjectDeleteAction(String objectId, ObjectType objectType) {
        return id -> employeeService.deleteEmployeeByObject(new EmployeeByObjectId(id, objectId, objectType));
    }

    public DeleteAction getDeleteAction() {
        return id -> {
            try {
                employeeService.deleteEmployee(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }

    public List<String> getLinkedEmployeeList(String objectId, ObjectType objectType) {
        return employeeService.getLinkedEmployeeIdList(objectId, objectType);
    }

    public SaveAction<EmployeeLinkDto> getEmployeeByObjectDtoSaveAction() {
        return (newDto, originalDto) -> {
            try {
                employeeService.saveEmployeeByObject(newDto);
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public SaveAction<EmployeeDto> getEmployeeDtoSaveAction(EmployeeNewDialog employeeNewDialog) {
        return (newDto, originalDto) -> {
            try {
                if (newDto.getLineManagerUserDto() != null && newDto.getLineManagerUserDto().getId() == null) {
                    newDto.setLineManagerUserDto(null);
                }
                if (originalDto.getId() != null) {
                    employeeService.updateEmployee(newDto, originalDto);
                    UI.getCurrent().navigate(EmployeeView.ROUTE);
                } else {
                    employeeService.saveEmployee(newDto);
                    if (employeeNewDialog != null) {
                        employeeNewDialog.showWarning(false);
                        employeeNewDialog.close();
                        UI.getCurrent().navigate(EmployeeDetailView.ROUTE.concat("/").concat(newDto.getId()));
                    }
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    public String getEmployeeName(String employeeId) {
        return employeeService.getEmployeeName(employeeId);
    }

    public String getEmployeeName(EmployeeDto employeeDto) {
        return employeeService.getEmployeeName(employeeDto);
    }

    public List<EmployeeDto> getEmployeeList(EmployeeDto dto) {
        List<UserDto> userList = userService.findUserList();
        List<EmployeeDto> employeeList = employeeService.findValidEmployeeList();
        for (UserDto user : userList) {
            if (employeeList.contains(user.getEmployee()) && !Objects.equals(user.getEmployee(), dto)) {
                employeeList.remove(user.getEmployee());
            }
        }
        return employeeList;
    }
}
