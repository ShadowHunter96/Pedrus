package cz.bbn.cerberus.role.ui;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.role.RoleService;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.role.ui.component.RoleGridComponent;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;

import java.util.List;


@Route(value = RoleView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ROLE_VIEW)
@Slf4j
public class RoleView extends AppView implements RoleViewListener {

    public static final String ROUTE = "role-list";
    public final RoleService roleService;
    private final AppEnv appEnv;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public RoleView(RoleService roleService, AppEnv appEnv, EntityNewComponentOperation entityNewComponentOperation) {
        this.roleService = roleService;
        this.appEnv = appEnv;
        this.entityNewComponentOperation = entityNewComponentOperation;
        setSizeFull();
    }

    public Page<RoleEntity> getPage(int page, int size, List<Sort.Order> orderList) {
        return roleService.findAll(page, size, orderList);
    }

    protected void initView() {
        removeAll();
        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Role list"), Permission.ROLE_EDIT,
                RoleDetailView.ROUTE, Transl.get("Add role"), entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY);
        card.setId(RobotFrameworkVariables.ROLE_VIEW_CARD_ID.getValue());
        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation));

        RoleGridComponent roleGridComponent = new RoleGridComponent(getDeleteAction(), appEnv, getItemsAction());
        roleGridComponent.initGrid();
        roleGridComponent.loadData();

        card.add(roleGridComponent);
        add(card);
    }

    private ItemsAction<RoleEntity> getItemsAction() {
        return (query, orderList) ->
                roleService.findAll(query.getPage(), query.getPageSize(), orderList);
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                roleService.deleteRole(id);
                SuccessNotification.show(TextValues.ITEM_WAS_DELETED, appEnv);
            } catch (SystemException ex) {
                log.error("SystemException", ex);
                ErrorNotification.show(ex, appEnv);
            }
        };
    }

    @Override
    protected void onAttach(AttachEvent attachEvent) {
        super.onAttach(attachEvent);
        initView();
    }
}
