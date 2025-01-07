package cz.bbn.cerberus.dssetting.ui.components;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dssetting.dto.DsSettingSimpleDto;
import cz.bbn.cerberus.dssetting.ui.DsSettingDetailView;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class DsSettingGridComponent extends AppInfiniteGrid<DsSettingSimpleDto> {


    public DsSettingGridComponent(DeleteAction deleteAction, AppEnv appEnv,
                                  ItemsAction<DsSettingSimpleDto> itemsAction) {
        super(deleteAction, appEnv, itemsAction);
        initComponent();
    }

    void initComponent() {
        setSizeFull();
        addColumn(DsSettingSimpleDto::getId).setHeader(Transl.get("Recipient ds id")).setSortable(true).setKey("id");
        addColumn(DsSettingSimpleDto::getName).setHeader(Transl.get("Name")).setSortable(true).setKey("name");
        addColumn(DsSettingSimpleDto::getDescription).setHeader(Transl.get("Description"))
                .setSortable(true).setKey("description");
        addColumn(new ComponentRenderer<>(dsSettingSimpleDto ->
                VaadinComponents.getCheckUncheckLayoutNullTrue(dsSettingSimpleDto.getDeleted())))
                .setHeader(Transl.get("Deleted"))
                .setWidth(VaadinValues.COLUMN_ICON_SIZE_MEDIUM)
                .setFlexGrow(0)
                .setSortable(true).setKey("deleted");

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addItemDoubleClickListener(event -> gridClicked(event.getItem().getId()));
    }

    private HorizontalLayout getGridButtons(DsSettingSimpleDto clickedItem) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("ds setting");
        AppGridDataVariables dataVariables = new AppGridDataVariables(
                Permission.DS_SETTINGS_EDIT, Permission.DS_SETTINGS_DELETE_RECOVERY, clickedItem.getId(), "",
                DsSettingDetailView.ROUTE, DomainEnum.DS_SETTING_DOMAIN_NAME.getValue(), clickedItem.getDeleted());
        return getSimpleGridButtons(dataVariables, stringVariables, this);
    }

    private void gridClicked(String code) {
        UI.getCurrent().navigate(DsSettingDetailView.ROUTE + "/" + code);
    }
}
