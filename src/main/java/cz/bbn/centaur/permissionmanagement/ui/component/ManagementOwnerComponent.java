package cz.bbn.cerberus.permissionmanagement.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.management.OwnerService;
import cz.bbn.cerberus.management.ui.component.OwnerGridComponent;
import cz.bbn.cerberus.permissionmanagement.dto.OwnerEntityDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ManagementOwnerComponent extends VerticalLayout {

    private final ListService listService;
    private final OwnerService ownerService;
    private final AppEnv appEnv;

    private final ComboBox<ObjectType> objectType = new ComboBox<>(Transl.get("Domain"));
    private HorizontalLayout roleGridLayout = new HorizontalLayout();
    private HorizontalLayout firstLine = new HorizontalLayout();
    private VerticalLayout roleDiv = new VerticalLayout();
    private VerticalLayout ownerLayout = new VerticalLayout();
    private Map<ObjectType, List<OwnerEntityDto>> objectTypeMap = new HashMap<>();
    private OwnerGridComponent ownerGridComponent;
    private MultiSelectComboBox<UserDto> user;

    public ManagementOwnerComponent(ListService listService, OwnerService ownerService, AppEnv appEnv) {
        this.listService = listService;
        this.ownerService = ownerService;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        user = new MultiSelectComboBox<>(Transl.get("Users"));
        user.setItems(listService.getUserDtoList());
        user.setItemLabelGenerator(UserDto::getName);
        user.setWidthFull();
        user.addValueChangeListener(event -> {
            Set<String> roleSet = new HashSet<>();
            event.getValue().forEach(userDto -> {
                String[] roles = userDto.getUserRoles().split(";");
                Arrays.stream(roles).forEach(roleSet::add);
            });
            if(!event.getValue().isEmpty()) {
                objectType.setVisible(true);
                roleGridLayout.setVisible(true);
                firstLine.setVisible(true);
                generateUserGrid(event.getValue(), roleSet);
                if(objectType.getValue() == null) {
                    objectType.setValue(ObjectType.SUBJECT);
                }
                generateUserOwnerGrid(event.getValue());
            }else{
                objectType.setVisible(false);
                roleGridLayout.setVisible(false);
                firstLine.setVisible(false);
            }
            objectTypeMap.clear();
        });
        setupComponents();
        this.add(user);
        this.add(roleDiv);
        roleGridLayout.setVisible(false);
        objectType.setItems(ObjectType.getEntityType());
        objectType.setItemLabelGenerator(objectType -> Transl.get(objectType.name()));
        objectType.addValueChangeListener(event -> generateUserOwnerGrid(user.getValue()));
        objectType.setVisible(false);
        ownerLayout.add(objectType);
        this.add(ownerLayout);
    }

    private void setupComponents() {
        roleGridLayout.setMargin(false);
        roleGridLayout.setPadding(false);
        roleGridLayout.setSpacing(false);
        roleGridLayout.setHeightFull();
        roleGridLayout.setAlignItems(Alignment.START);
        roleGridLayout.setDefaultVerticalComponentAlignment(Alignment.START);

        roleDiv.setSizeFull();
        roleDiv.setMargin(false);
        roleDiv.setPadding(false);
        roleDiv.setSpacing(false);
        roleDiv.getElement().getStyle().set("overflow", "auto");

        ownerLayout.setMargin(false);
        ownerLayout.setPadding(false);
        ownerLayout.setSpacing(false);
    }

    private void generateUserGrid(Set<UserDto> userDtoSet, Set<String> roleSet) {
        roleDiv.removeAll();
        roleGridLayout.removeAll();
        generateFirstColumn(roleGridLayout, roleSet);

        for (UserDto userDto : userDtoSet) {
            generateUserColumn(userDto, roleSet, roleGridLayout);
        }
        roleDiv.add(generateFirstLine(userDtoSet), roleGridLayout);
    }

    private void generateUserOwnerGrid(Set<UserDto> userDtoSet) {
        List<OwnerEntityDto> list = ownerService.getEntityOwnerList(userDtoSet, objectType.getValue());
        objectTypeMap.put(objectType.getValue(), list);

        if(ownerLayout.getComponentCount() > 1) {
            ownerLayout.remove(ownerGridComponent);
        }
        ownerGridComponent = new OwnerGridComponent(appEnv, userDtoSet, getOwnerEntityDtoItemsAction(list));
        ownerLayout.add(ownerGridComponent);
    }

    ItemsAction<OwnerEntityDto> getOwnerEntityDtoItemsAction(List<OwnerEntityDto> list){
        return (query, orderList) -> new PageImpl<>(list.stream().skip((query.getPage()) * query.getPageSize())
                .limit(query.getPageSize())
                .toList(), PageRequest.of(query.getPage(), query.getPageSize()), list.size());
    }

    private HorizontalLayout generateFirstLine(Set<UserDto> userDtoSet) {
        firstLine = new HorizontalLayout();
        firstLine.setMargin(false);
        firstLine.setPadding(false);
        firstLine.setSpacing(false);
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setPadding(false);
        verticalLayout.setSpacing(false);
        verticalLayout.setWidth("21em");
        verticalLayout.setMinWidth("21em");
        verticalLayout.setHeight("2em");
        Div emptyDiv = new Div();
        emptyDiv.addClassName("grid-block");
        verticalLayout.add(emptyDiv);
        verticalLayout.getElement().getStyle().set("position", "sticky").set("top", "0").set("left", "0")
                .set("background-color", "white").set("z-index", "1");
        firstLine.add(verticalLayout);
        firstLine.getStyle().set("position", "sticky").set("top", "0")
                .set("background-color", "white").set("z-index", "1");

        for (UserDto userDto : userDtoSet) {
            VerticalLayout outerLayout = new VerticalLayout();
            outerLayout.setAlignItems(Alignment.CENTER);
            outerLayout.setHeight("2em");
            outerLayout.setMargin(false);
            outerLayout.setPadding(false);
            outerLayout.setSpacing(false);
            outerLayout.setWidth("10em");
            outerLayout.setMinWidth("10em");
            outerLayout.setVisible(true);
            Div roleName = new Div();
            roleName.add(new Label(userDto.getName()));
            roleName.addClassNames("grid-block", "align-center-simple");
            outerLayout.add(roleName);
            firstLine.add(outerLayout);
        }
        return firstLine;
    }

    private void generateFirstColumn(HorizontalLayout gridLayout, Set<String> roleSet) {

        VerticalLayout firstColumn = new VerticalLayout();
        firstColumn.setMargin(false);
        firstColumn.setPadding(false);
        firstColumn.setSpacing(false);
        firstColumn.setWidth("21em");
        firstColumn.setMinWidth("21em");
        firstColumn.getElement().getStyle().set("position", "sticky").set("left", "0")
                .set("background-color", "white").set("z-index", "1").set("margin-top", "calc(-1.5em - 10px)");

        Div emptyDiv = new Div();
        emptyDiv.setMinWidth("calc(21em + 10px)");
        emptyDiv.setMinHeight("calc(1.5em + 13px)");
        emptyDiv.getElement().getStyle().set("position", "sticky").set("top", "0").set("left", "0")
                .set("background-color", "white").set("margin-top", "-3px");
        firstColumn.add(emptyDiv);

        Div domainName = new Div();
        domainName.addClassName("grid-block");
        domainName.add(new Label(Transl.get("Roles")));
        firstColumn.add(domainName);
        roleSet.forEach(role -> {
            Div itemDiv = new Div();
            itemDiv.addClassName("grid-block");
            Label item = new Label(role);
            item.getElement().getStyle().set("padding-left", "4em");
            itemDiv.add(item);
            firstColumn.add(itemDiv);
        });
        gridLayout.add(firstColumn);
    }

    private void generateUserColumn(UserDto userDto, Set<String> roleSet,HorizontalLayout gridLayout) {
        VerticalLayout column = new VerticalLayout();
        column.setAlignItems(Alignment.CENTER);
        column.setHeightFull();
        column.setMargin(false);
        column.setPadding(false);
        column.setSpacing(false);
        column.setWidth("10em");
        column.setMinWidth("10em");
        Div domainDiv = new Div();
        domainDiv.addClassNames("grid-block", "align-center-simple");
        column.add(domainDiv);
        roleSet.forEach(role -> {
            Div itemDiv = new Div();
            itemDiv.addClassNames("grid-block", "align-center-simple");
            Checkbox item = new Checkbox();
            item.setReadOnly(true);
            item.setValue(userDto.getUserRoles().contains(role));
            itemDiv.add(item);
            column.add(itemDiv);
        });
        gridLayout.add(column);
    }

    public OwnerGridComponent getOwnerGridComponent2() {
        return ownerGridComponent;
    }

    public ComboBox<ObjectType> getObjectType() {
        return objectType;
    }

    public MultiSelectComboBox<UserDto> getUser() {
        return user;
    }
}
