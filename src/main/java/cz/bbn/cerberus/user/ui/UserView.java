package cz.bbn.cerberus.user.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.azure.AzureGraphService;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.dto.UserFilterDto;
import cz.bbn.cerberus.user.ui.component.UserFilterDtoComponent;
import cz.bbn.cerberus.user.ui.component.UserGridComponent;
import lombok.extern.slf4j.Slf4j;

@Route(value = UserView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.USER_EDIT)
@Slf4j
public class UserView extends AppView {

    public static final String ROUTE = "user-list";

    private final UserService userService;
    private final AzureGraphService azureGraphService;
    private final EmployeeComponentOperation employeeComponentOperation;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public UserView(UserService userService, AzureGraphService azureGraphService,
                    EmployeeComponentOperation employeeComponentOperation, AppEnv appEnv,
                    EntityNewComponentOperation entityNewComponentOperation) {
        this.userService = userService;
        this.azureGraphService = azureGraphService;
        this.employeeComponentOperation = employeeComponentOperation;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        initComponent();
    }


    public void initComponent() {
        removeAll();
        setSizeFull();
        setId(RobotFrameworkVariables.USER_VIEW_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        UserFilterDtoComponent userFilterDtoComponent = new UserFilterDtoComponent(search);
        UserGridComponent userGridComponent = new UserGridComponent(getDeleteAction(), appEnv,
                getItemsAction(userFilterDtoComponent), azureGraphService, employeeComponentOperation);
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("User list"),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY
        );

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));
        card.setId(RobotFrameworkVariables.SUBJECT_VIEW_CARD_ID.getValue());
        card.add(userFilterDtoComponent);
        userGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> userGridComponent.loadData());
        card.add(userGridComponent);

        add(card);
    }

    private ItemsAction<UserDto> getItemsAction(UserFilterDtoComponent filterDtoComponent) {
        return (query, orderList) -> {
            UserFilterDto userFilterDto = filterDtoComponent.getUserFilterDto();
            userFilterDto.setPage(query.getPage());
            userFilterDto.setSize(query.getPageSize());
            userFilterDto.setOrderList(orderList);
            return userService.findUserPageDto(userFilterDto);
        };

    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                userService.deleteUser(Long.valueOf(id));
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
