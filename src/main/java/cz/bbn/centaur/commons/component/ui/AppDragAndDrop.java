package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.AbstractField;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.grid.ItemClickEvent;
import com.vaadin.flow.component.grid.dnd.GridDragEndEvent;
import com.vaadin.flow.component.grid.dnd.GridDragStartEvent;
import com.vaadin.flow.component.grid.dnd.GridDropEvent;
import com.vaadin.flow.component.grid.dnd.GridDropMode;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.provider.ListDataProvider;
import com.vaadin.flow.data.provider.Query;
import com.vaadin.flow.data.value.ValueChangeMode;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.custompermission.dto.PermUserDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class AppDragAndDrop<T> extends VerticalLayout {

    private final Set<T> rightSet;
    private final Set<T> leftSet;
    private final String columnId;
    private final String columnName;
    private final String titleLeft;
    private final String titleRight;
    private final boolean canEdit;

    private Set<T> draggedItemSet;
    private Grid<T> gridRight;
    private Grid<T> gridLeft;
    private final Class<T> clazz;

    protected AppDragAndDrop(Set<T> rightSet, Set<T> leftSet,
                             String columnId, String columnName,
                             String titleLeft, String titleRight,
                             Class<T> clazz, boolean canEdit) {
        this.rightSet = rightSet;
        this.leftSet = leftSet;
        this.columnId = columnId;
        this.columnName = columnName;
        this.titleLeft = titleLeft;
        this.titleRight = titleRight;
        this.clazz = clazz;
        this.canEdit = canEdit;
        setHeightFull();
        initComponent();
    }

    public void initComponent() {
        addDragAndDrop();
    }

    public abstract String getSearchCompareVariable(T object);

    public void onChangeAction(Set<T> rightSet) {
        // not supported yet
    }

    public void setGridsHeight(String height) {
        gridRight.setHeight(height);
        gridLeft.setHeight(height);
    }

    public void setGridsWidth(String width) {
        gridRight.setWidth(width);
        gridLeft.setWidth(width);
    }

    private void addDragAndDrop() {
        setMargin(false);
        setPadding(false);
        setAlignItems(Alignment.CENTER);
        HorizontalLayout horizontal = new HorizontalLayout();
        horizontal.setDefaultVerticalComponentAlignment(Alignment.CENTER);
        horizontal.setAlignItems(Alignment.CENTER);
        horizontal.setHeightFull();
        horizontal.addClassName("permission-horizontal-layout");
        horizontal.setMargin(false);
        horizontal.setPadding(false);
        draggedItemSet = new HashSet<>();

        gridRight = new Grid<>(clazz);
        gridLeft = new Grid<>(clazz);
        gridRight.addClassName("drag-and-drop-grid");
        gridLeft.addClassName("drag-and-drop-grid");
        gridLeft.addClassName(RobotFrameworkVariables.LEFT_ROLE_LIST_CLASS.getValue());
        gridLeft.setSelectionMode(Grid.SelectionMode.MULTI);
        gridLeft.setItems(leftSet);

        gridLeft.addDragStartListener((GridDragStartEvent<T> event) ->
                addDragStartItem(event, gridRight)
        );

        gridLeft.addDragEndListener((GridDragEndEvent<T> event) -> {
            draggedItemSet = new HashSet<>();
            gridRight.setDropMode(GridDropMode.ON_GRID);
        });
        gridLeft.setColumns(columnId);
        gridLeft.getColumnByKey(columnId).setHeader(getGridColumn(gridLeft, leftSet));
        gridLeft.setRowsDraggable(true);

        if (canEdit) {
            gridLeft.addDropListener((GridDropEvent<T> event) -> {
                removeItems(gridRight, gridLeft, draggedItemSet);
                gridLeft.getDataProvider().refreshAll();
                gridRight.getDataProvider().refreshAll();
            });
        }
        gridLeft.getElement().executeJs(
                "this.getElementsByTagName(\"vaadin-grid-flow-selection-column\")[0].hidden = true;");
        gridLeft.addItemClickListener(event -> addMultiselect(event, gridLeft, gridRight));


        gridRight.addClassName(RobotFrameworkVariables.RIGHT_ROLE_LIST_CLASS.getValue());
        gridRight.setColumns(columnId);
        gridRight.getColumnByKey(columnId).setHeader(getGridColumn(gridRight, rightSet));
        gridRight.setSelectionMode(Grid.SelectionMode.MULTI);
        gridRight.setRowsDraggable(true);
        gridRight.setItems(rightSet);

        if (canEdit) {
            gridRight.addDropListener((GridDropEvent<T> event) -> {
                addItems(gridRight, gridLeft, draggedItemSet);
                gridLeft.getDataProvider().refreshAll();
                gridRight.getDataProvider().refreshAll();
            });
        }
        gridRight.setRowsDraggable(true);

        gridRight.addDragStartListener((GridDragStartEvent<T> event) ->
                addDragStartItem(event, gridLeft)
        );

        gridRight.addDragEndListener((GridDragEndEvent<T> event) -> {
            draggedItemSet = new HashSet<>();
            gridLeft.setDropMode(GridDropMode.ON_GRID);
        });
        gridRight.getElement().executeJs(
                "this.getElementsByTagName(\"vaadin-grid-flow-selection-column\")[0].hidden = true;");
        gridRight.addItemClickListener(event ->
                addMultiselect(event, gridRight, gridLeft));


        VerticalLayout buttonLayout = new VerticalLayout();

        Button add = VaadinComponents.getButton(VaadinIcon.ANGLE_DOUBLE_RIGHT.create());
        add.getElement().setProperty(TextValues.TITLE, Transl.get("Add"));
        add.addClassName(RobotFrameworkVariables.ADD_BUTTON_CLASS.getValue());
        add.addClickListener(event -> {
            addItems(gridRight, gridLeft, gridLeft.getSelectionModel().getSelectedItems());
            draggedItemSet.clear();
        });

        Button remove = VaadinComponents.getButton(VaadinIcon.ANGLE_DOUBLE_LEFT.create());
        remove.getElement().setProperty(TextValues.TITLE, Transl.get("Remove"));
        remove.addClassName(RobotFrameworkVariables.REMOVE_BUTTON_CLASS.getValue());
        remove.addClickListener(event -> {
            removeItems(gridRight, gridLeft, gridRight.getSelectionModel().getSelectedItems());
            draggedItemSet.clear();
        });

        if (!canEdit) {
            add.setEnabled(false);
            remove.setEnabled(false);
        }

        buttonLayout.add(add, remove);
        buttonLayout.setAlignItems(Alignment.CENTER);
        buttonLayout.setMaxWidth("20em");

        VerticalLayout unassignedLayout = new VerticalLayout();

        unassignedLayout.setHeightFull();
        Label labelUnassigned = new Label(titleLeft);
        unassignedLayout.add(labelUnassigned);
        unassignedLayout.add(gridLeft);
        unassignedLayout.setClassName("no-padding-margin");
        labelUnassigned.getElement().getStyle().set("margin-top", "var(--lumo-space-m)").set("font-weight", "600")
                .set("font-size", "var(--lumo-font-size-xl)");

        VerticalLayout assignedLayout = new VerticalLayout();
        assignedLayout.setHeightFull();
        Label labelOwnedPermissions = new Label(titleRight);
        assignedLayout.add(labelOwnedPermissions);
        assignedLayout.add(gridRight);
        assignedLayout.setClassName("no-padding-margin");
        labelOwnedPermissions.getElement().getStyle().set("margin-top", "var(--lumo-space-m)").set("font-weight", "600")
                .set("font-size", "var(--lumo-font-size-xl)");

        horizontal.add(unassignedLayout, buttonLayout, assignedLayout);
        this.add(horizontal);
    }

    private void addMultiselect(ItemClickEvent<T> event, Grid<T> gridFirst, Grid<T> gridSecond) {
        if (event.isShiftKey()) {
            T lastSelected =
                    gridFirst.getSelectedItems().stream().toList().get(gridFirst.getSelectedItems().size() - 1);
            List<T> items = gridFirst.getDataProvider().fetch(new Query<>()).toList();
            List<T> selectedList = items.stream().filter(dto ->
                    dto.equals(event.getItem()) || dto.equals(lastSelected)).toList();
            if (selectedList.size() < 2) {
                selectRowAction(event, gridFirst, gridSecond);
                return;
            }
            int startIndex = items.indexOf(selectedList.get(0));
            int endIndex = items.indexOf(selectedList.get(1));
            gridFirst.deselectAll();
            draggedItemSet.clear();
            List<T> selectedItems;
            if (startIndex == endIndex) {
                selectedItems = new ArrayList<>();
            } else {
                selectedItems = items.subList(startIndex, endIndex != items.size() - 1 ? endIndex + 1 : endIndex + 1);
            }
            selectedItems.forEach(dto -> {
                gridFirst.select(dto);
                draggedItemSet.add(dto);
            });
        } else {
            selectRowAction(event, gridFirst, gridSecond);
        }
    }

    private HorizontalLayout getGridColumn(Grid<T> grid, Set<T> set) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setAlignItems(Alignment.CENTER);
        horizontalLayout.setHeight("2.2em");
        TextField search = getSearchField(grid, set);
        search.setClassName("search-field");
        Label column = new Label(columnName);
        horizontalLayout.add(column, search);
        return horizontalLayout;
    }

    private void addItems(Grid<T> gridRight, Grid<T> gridLeft,
                          Set<T> itemSet) {
        if (itemSet == null) {
            return;
        }

        removeItemFromList(leftSet, gridLeft, itemSet);
        addItemToList(rightSet, gridRight, itemSet);

    }

    private void removeItems(Grid<T> gridRight, Grid<T> gridLeft,
                             Set<T> itemSet) {
        if (itemSet == null) {
            return;
        }

        if (clazz.equals(PermUserDto.class)) {
            Set<T> allowedItemSet = new HashSet<>();
            for (T item : itemSet) {
                PermUserDto userDto = (PermUserDto) item;
                if (!userDto.isPermanent()) {
                    allowedItemSet.add(item);
                }
            }
            itemSet = allowedItemSet;
        }

        addItemToList(leftSet, gridLeft, itemSet);
        removeItemFromList(rightSet, gridRight, itemSet);
    }

    private void addItemToList(
            Set<T> permissions, Grid<T> grid, Set<T> itemSet
    ) {
        ListDataProvider<T> sourceDataRightProvider =
                (ListDataProvider<T>) grid.getDataProvider();
        List<T> sourceRightItems = new ArrayList<>(sourceDataRightProvider.getItems());
        sourceRightItems.addAll(itemSet);
        grid.setItems(sourceRightItems);
        permissions.addAll(itemSet);
        onChangeAction(rightSet);
    }

    private void removeItemFromList(
            Set<T> permissions, Grid<T> grid, Set<T> itemList
    ) {
        ListDataProvider<T> sourceDataProvider =
                (ListDataProvider<T>) grid.getDataProvider();
        Collection<T> sourceItems = sourceDataProvider.getItems();
        sourceItems.removeAll(itemList);
        grid.setItems(sourceItems);
        permissions.removeAll(itemList);
        onChangeAction(rightSet);
    }

    private TextField getSearchField(Grid<T> grid, Set<T> set) {
        TextField search = new TextField();
        search.setValueChangeMode(ValueChangeMode.EAGER);
        search.addValueChangeListener((AbstractField.ComponentValueChangeEvent<TextField, String> event) -> {
            if (StringUtils.isEmpty(event.getValue())) {
                grid.setItems(set);
            } else {
                grid.setItems(set
                        .stream()
                        .filter(item ->
                                getSearchCompareVariable(item).toLowerCase().contains(event.getValue().toLowerCase()))
                        .collect(Collectors.toSet()));
            }

            grid.getDataProvider().refreshAll();
            draggedItemSet.clear();
        });

        return search;
    }

    private void addDragStartItem(GridDragStartEvent<T> event, Grid<T> grid) {
        if (clazz.equals(PermUserDto.class)) {
            PermUserDto userDto = (PermUserDto) event.getDraggedItems().get(0);
            if (!userDto.isPermanent()) {
                draggedItemSet.add(event.getDraggedItems().get(0));
                grid.setDropMode(GridDropMode.ON_GRID);
            }
        } else {
            draggedItemSet.add(event.getDraggedItems().get(0));
            grid.setDropMode(GridDropMode.ON_GRID);
        }
    }

    private void selectRowAction(ItemClickEvent<T> event, Grid<T> grid, Grid<T> gridForDeselect) {
        T item = event.getItem();
        if (!gridForDeselect.getSelectedItems().isEmpty()) {
            gridForDeselect.deselectAll();
            draggedItemSet.clear();
        }
        if (draggedItemSet.contains(event.getItem())) {
            draggedItemSet.remove(item);
            grid.deselect(event.getItem());
        } else {
            grid.select(event.getItem());
            draggedItemSet.add(item);
        }
    }

}
