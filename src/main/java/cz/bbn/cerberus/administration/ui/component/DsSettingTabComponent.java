package cz.bbn.cerberus.administration.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabSimpleComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dssetting.DsSettingComponentOperation;
import cz.bbn.cerberus.dssetting.DsSettingService;
import cz.bbn.cerberus.dssetting.dto.DsSettingFilterDto;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.ui.components.DsSettingFilterComponent;
import cz.bbn.cerberus.dssetting.ui.components.DsSettingGridComponent;
import cz.bbn.cerberus.dssetting.ui.components.DsSettingNewDialog;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class DsSettingTabComponent extends TabSimpleComponent {

    public static final int TAB_INDEX = 6;

    private final DsSettingService dsSettingService;
    private final DsSettingComponentOperation dsSettingComponentOperation;
    private final AppEnv appEnv;

    private DsSettingGridComponent dsSettingGridComponent;

    public DsSettingTabComponent(AppEnv appEnv, DsSettingService dsSettingService,
                                 DsSettingComponentOperation dsSettingComponentOperation) {
        this.dsSettingService = dsSettingService;
        this.dsSettingComponentOperation = dsSettingComponentOperation;
        this.appEnv = appEnv;
        initComponent();
    }

    void initComponent() {
        setSizeFull();
        setId(RobotFrameworkVariables.DS_SETTING_TAB_CARD_ID.getValue());

        Button search = VaadinComponents.getSearchButton();
        DsSettingFilterComponent dsSettingFilterComponent = new DsSettingFilterComponent(search);
        this.add(dsSettingFilterComponent);

        dsSettingGridComponent = new DsSettingGridComponent(
                getDeleteAction(), appEnv, getItemsAction(dsSettingFilterComponent));

        dsSettingGridComponent.loadData();
        search.addClickListener(buttonClickEvent -> dsSettingGridComponent.loadData());
        this.add(dsSettingGridComponent);
    }

    @Override
    public Button getFooterButton() {
        if (SecurityUtils.hasPermission(Permission.DS_SETTINGS_EDIT)) {
            Button addNew = VaadinComponents.getNewButton(Transl.get("Add ds settings"));
            addNew.addClickListener(e ->
                    new DsSettingNewDialog(dsSettingGridComponent, dsSettingComponentOperation).open());
            return addNew;
        }
        return null;
    }

    private ItemsAction<DsSettingSimpleDto> getItemsAction(DsSettingFilterComponent filterComponent) {
        return (query, orderList) -> {
            DsSettingFilterDto filter = filterComponent.getDsSettingFilterDto();
            filter.setPage(query.getPage());
            filter.setSize(query.getPageSize());
            filter.setOrderList(orderList);
            return dsSettingService.findDsSettingDtoPage(filter);
        };
    }

    private DeleteAction getDeleteAction() {
        return id -> {
            try {
                dsSettingService.deleteDsSetting(id);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex.getMessage(), appEnv);
            }
        };
    }
}
