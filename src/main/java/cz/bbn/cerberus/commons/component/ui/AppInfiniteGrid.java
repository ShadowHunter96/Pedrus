package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.GridSortOrder;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.SortDirection;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.dialog.DeleteConfirmDialog;
import cz.bbn.cerberus.commons.component.ui.dialog.WarningListDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.EditEvent;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ListAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.UnlinkAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.translation.Transl;
import org.springframework.data.domain.Sort;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public abstract class AppInfiniteGrid<T> extends Grid<T> {

    private final AppEnv appEnv;
    private final DeleteAction deleteAction;
    private final ItemsAction<T> itemsAction;

    protected AppInfiniteGrid(AppEnv appEnv, ItemsAction<T> itemsAction) {
        this.itemsAction = itemsAction;
        this.deleteAction = null;
        this.appEnv = appEnv;
        setMinHeight("15em");
    }

    protected AppInfiniteGrid(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<T> itemsAction) {
        this.deleteAction = deleteAction;
        this.appEnv = appEnv;
        this.itemsAction = itemsAction;
        setMinHeight("15em");
    }

    public void loadData() {
        setItems(query ->
                itemsAction.getItems(query, getOrderList()).stream()
        );
    }

    public void deleteItem(String appId) throws SystemException {
        Optional.ofNullable(deleteAction).orElseThrow(
                () -> new SystemException(ErrorCode.ACTION_NOT_SUPPORTED)).deleteItem(appId);
        loadData();
    }

    public HorizontalLayout getGridButtons(
            AppGridDataVariables dataVariables, AppGridStringVariables stringVariables,
            ListAction<String> listAction, AppInfiniteGrid<?> appInfiniteGrid) {

        HorizontalLayout buttons = new HorizontalLayout();

        if (SecurityUtils.hasPermission(dataVariables.getEditPermission())) {
            Button edit = VaadinComponents.getEditButton();
            AppUtils.addRfClassToGridButton(edit, dataVariables.getDtoId());
            edit.addClickListener(buttonClickEvent ->
                    UI.getCurrent().navigate(dataVariables.getRoute() + "/" + dataVariables.getDtoId()));
            edit.getElement().setProperty(TextValues.TITLE, Transl.get(stringVariables.getEditTitle()));
            buttons.add(edit);
        }

        if (SecurityUtils.hasPermission(dataVariables.getDeletePermission())
                && !Boolean.TRUE.equals(dataVariables.getHideDeleteButton())) {
            Button delete = VaadinComponents.getDeleteButton();
            AppUtils.addRfClassToGridButton(delete, dataVariables.getDtoId());
            delete.addClickListener(buttonClickEvent -> {
                List<String> templateList = listAction.getList(dataVariables.getDtoId());
                if (templateList.isEmpty()) {
                    DeleteConfirmDialog deleteConfirmDialog =
                            new DeleteConfirmDialog(appInfiniteGrid, dataVariables.getDtoId(),
                                    Transl.get(stringVariables.getDeleteConfirm(), dataVariables.getDtoName()),
                                    appEnv, true);
                    deleteConfirmDialog.open();
                } else {
                    WarningListDialog warningListDialog = new WarningListDialog(
                            templateList, Transl.get(stringVariables.getDeleteWarningFirst()),
                            Transl.get(stringVariables.getDeleteWarningSecond()),
                            Transl.get(stringVariables.getDeleteWarningThird()));
                    warningListDialog.open();
                }
            });
            delete.getElement().setProperty(TextValues.TITLE, Transl.get(stringVariables.getDeleteTitle()));
            buttons.add(delete);
        }

        return buttons;
    }

    public HorizontalLayout getSimpleGridButtons(
            AppGridDataVariables dataVariables,
            AppGridStringVariables stringVariables,
            AppInfiniteGrid<?> appInfiniteGrid,
            EditEvent editEvent) {
        return getSimpleGridButtons(dataVariables, stringVariables, appInfiniteGrid, false);
    }

    public HorizontalLayout getSimpleGridButtons(
            AppGridDataVariables dataVariables,
            AppGridStringVariables stringVariables,
            AppInfiniteGrid<?> appInfiniteGrid,
            boolean recoveryItem) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");


        if (SecurityUtils.hasPermission(dataVariables.getDeletePermission())
                && deleteAction != null && !Boolean.TRUE.equals(dataVariables.getHideDeleteButton())) {
            Button delete = VaadinComponents.getDeleteButton(recoveryItem);
            AppUtils.addRfClassToGridButton(delete, dataVariables.getDtoId());
            delete.addClickListener(buttonClickEvent ->
                    getDeleteAction(dataVariables, stringVariables, appInfiniteGrid, recoveryItem));

            delete.getElement().setProperty(TextValues.TITLE,
                    stringVariables.getDeleteTitle() == null ?
                            recoveryItem ? Transl.get("Recovery ".concat(stringVariables.getItem()))
                                    : Transl.get("Delete ".concat(stringVariables.getItem()))
                            : Transl.get(stringVariables.getDeleteTitle()));
            buttons.add(delete);
        }

        return buttons;

    }

    public void getDeleteAction(AppGridDataVariables dataVariables, AppGridStringVariables stringVariables,
                                AppInfiniteGrid<?> appInfiniteGrid, boolean recoveryItem) {
        if ("".equals(dataVariables.getObjectName()) || SecurityUtils.hasCustomPermission(
                dataVariables.getObjectName(), dataVariables.getDtoId(),
                dataVariables.getDeletePermission().name())) {
            if (!recoveryItem) {
                DeleteConfirmDialog deleteConfirmDialog =
                        new DeleteConfirmDialog(appInfiniteGrid, dataVariables.getDtoId(),
                                Transl.get(stringVariables.getDeleteConfirm() == null ?
                                                "Are you sure you want to delete "
                                                        .concat(stringVariables.getItem()).concat(" {0}")
                                                : stringVariables.getDeleteConfirm(),
                                        dataVariables.getDtoName()), appEnv, true);
                deleteConfirmDialog.open();
            } else {
                appInfiniteGrid.deleteAction.deleteItem(dataVariables.getDtoId());
                appInfiniteGrid.loadData();
            }
        } else {
            ErrorNotification.show(Transl.get("You do not have permission to delete this item."), appEnv);
        }
    }

    public HorizontalLayout getSimpleGridButtons(
            AppGridDataVariables dataVariables, AppGridStringVariables stringVariables,
            AppInfiniteGrid<?> appInfiniteGrid) {
        return getSimpleGridButtons(dataVariables, stringVariables, appInfiniteGrid, false);
    }

    public HorizontalLayout getUnlinkGridButtons(UnlinkAction unlinkAction,
                                                 AppGridDataVariables dataVariables,
                                                 AppGridStringVariables stringVariables,
                                                 AppInfiniteGrid<?> appInfiniteGrid) {
        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        EditEvent editEvent = id ->
                buttonClickEvent -> UI.getCurrent().navigate(dataVariables.getRoute() + "/" + id);

        Button edit = dataVariables.getEditPermission() != null
                && SecurityUtils.hasPermission(
                dataVariables.getEditPermission()) ? VaadinComponents.getEditButton() :
                VaadinComponents.getViewButton();
        AppUtils.addRfClassToGridButton(edit, dataVariables.getDtoId());
        edit.addClickListener(editEvent.event(dataVariables.getDtoId()));
        edit.getElement().setProperty(TextValues.TITLE, stringVariables.getEditTitle() == null ?
                Transl.get("Edit ".concat(stringVariables.getItem())) : Transl.get(stringVariables.getEditTitle()));
        buttons.add(edit);

        if (SecurityUtils.hasPermission(dataVariables.getLinkPermission()) && unlinkAction != null) {
            Button unlink = VaadinComponents.getUnlinkButton();
            AppUtils.addRfClassToGridButton(unlink, dataVariables.getDtoId());
            unlink.addClickListener(buttonClickEvent -> {
                if ("".equals(dataVariables.getObjectName()) ||
                        SecurityUtils.hasCustomPermission(
                                dataVariables.getObjectName(), dataVariables.getDtoId(),
                                dataVariables.getLinkPermission().name())) {
                    unlinkAction.unlinkItem(dataVariables.getConnectionId());
                    appInfiniteGrid.loadData();
                } else {
                    ErrorNotification.show(Transl.get("You do not have permission to unlink this item."), appEnv);
                }
            });
            buttons.add(unlink);
        }
        return buttons;
    }


    public List<Sort.Order> getOrderList() {
        List<Sort.Order> orderList = new ArrayList<>();
        for (GridSortOrder<T> gridOrder : getSortOrder()) {
            if (gridOrder.getSorted() != null && gridOrder.getSorted().getKey() != null) {
                Sort.Order order =
                        new Sort.Order(checkDirection(gridOrder.getDirection()), gridOrder.getSorted().getKey());
                orderList.add(order);
            }
        }
        return orderList;
    }

    public void allowMultiselect() {
        this.setSelectionMode(Grid.SelectionMode.MULTI);
        this.getElement().executeJs(
                "this.getElementsByTagName(\"vaadin-grid-flow-selection-column\")[0].hidden = true;");
        this.addItemClickListener(event -> selectRowAction(event.getItem()));
    }

    private void selectRowAction(T dto) {
        if (this.getSelectedItems().contains(dto)) {
            this.deselect(dto);
        } else {
            this.select(dto);
        }
    }

    private Sort.Direction checkDirection(SortDirection oldDirection) {
        if (SortDirection.ASCENDING == oldDirection) {
            return Sort.Direction.ASC;
        } else {
            return Sort.Direction.DESC;
        }
    }

    public AppEnv getAppEnv() {
        return appEnv;
    }
}
