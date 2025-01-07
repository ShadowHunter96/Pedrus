package cz.bbn.cerberus.user.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.ui.component.UserDetailComponent;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;


@Route(value = UserDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.USER_EDIT)
@Slf4j
public class UserDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "user-detail";

    private final UserService userService;
    private final EmployeeComponentOperation employeeComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public UserDetailView(UserService userService, EmployeeComponentOperation employeeComponentOperation,
                          AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.userService = userService;
        this.employeeComponentOperation = employeeComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView(UserDto userDto) {
        UserDetailComponent userDetailComponent = new UserDetailComponent(userDto, getSaveAction(),
                SecurityUtils.hasPermission(Permission.USER_EDIT), employeeComponentOperation,
                appEnv, entityNewComponentOperation
        );
        this.add(userDetailComponent);
    }

    @Override
    public void setParameter(BeforeEvent beforeEvent, @OptionalParameter String param) {
        if (param != null) {
            try {
                UserDto dto = userService.getUser(Long.valueOf(param));
                refreshBreadcrumbText(StringUtils.isEmpty(dto.getName()) ? String.valueOf(dto.getId()) : dto.getName());
                initView(dto);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    private SaveAction<UserDto> getSaveAction() {
        return (dto, originalDto) -> {
            try {
                userService.updateUser(dto, originalDto);
                SuccessNotification.showSavingSuccess(appEnv);
                UI.getCurrent().navigate(UserView.ROUTE);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }
}
