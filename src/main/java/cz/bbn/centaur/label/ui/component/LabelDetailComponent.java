package cz.bbn.cerberus.label.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.provider.ListDataProvider;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.dto.LabelTableDto;
import cz.bbn.cerberus.label.dto.LabelType;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;


public class LabelDetailComponent extends VerticalLayout {

    private final LabelDto dto;
    private final Binder<LabelDto> binder;
    private final VerticalLayout componentLayout;
    private final AppEnv appEnv;

    private final ListDataProvider<ItemDto> dataProvider = new ListDataProvider<>(new ArrayList<>());

    public LabelDetailComponent(LabelDto dto, Binder<LabelDto> binder, VerticalLayout componentLayout, AppEnv appEnv) {
        this.dto = dto;
        this.binder = binder;
        this.componentLayout = componentLayout;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        FormLayout formLayout = new FormLayout();

        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        id.setValue(String.valueOf(dto.getId()));
        id.setEnabled(dto.getId() == null);
        binder.forField(id).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20))
                .bind(LabelDto::getId, LabelDto::setId);
        formLayout.add(id);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        binder.forField(name).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .bind(LabelDto::getName, LabelDto::setName);
        formLayout.add(name);

        ComboBox<LabelType> labelType = new ComboBox<>(Transl.get("Label Type"));
        labelType.setItems(LabelType.values());
        labelType.setItemLabelGenerator(labelType1 -> Transl.get(labelType1.name()));
        binder.forField(labelType).asRequired(Transl.get(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                .bind(LabelDto::getType, LabelDto::setType);
        formLayout.add(labelType);

        if (dto.getType() == LabelType.TABLE) {
            componentLayout.add(getGrid(dto));
        }

        if (!SecurityUtils.hasPermission(Permission.LABEL_EDIT)) {
            id.setReadOnly(true);
            name.setReadOnly(true);
            labelType.setReadOnly(true);
        }

        labelType.addValueChangeListener(event -> {
            componentLayout.removeAll();
            if (event.getValue() == LabelType.TABLE) {
                componentLayout.add(getGrid(dto));
            }
        });

        this.add(formLayout);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        componentLayout.setWidthFull();
        componentLayout.setPadding(false);
        componentLayout.setMargin(false);
        this.add(componentLayout);

        binder.setBean(dto);

    }

    private Grid<ItemDto> getGrid(LabelDto dto) {
        dataProvider.getItems().clear();
        dataProvider.getItems().addAll(dto.getTableValueList());
        Grid<ItemDto> grid = new Grid<>();
        grid.setWidth("20em");
        grid.addColumn(new ComponentRenderer<>(this::getRowLayout)).setHeader(getHeaderLayout());
        grid.setDataProvider(dataProvider);
        return grid;
    }

    private HorizontalLayout getRowLayout(ItemDto rowItemDto) {
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setAlignItems(FlexComponent.Alignment.CENTER);
        Label label = new Label(rowItemDto.getName());
        label.setWidth("90%");
        horizontalLayout.add(label);
        Button delete = VaadinComponents.getButton(VaadinIcon.TRASH.create());
        delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete"));
        delete.setId(rowItemDto.getId());
        delete.addClickListener(event -> {
            ItemDto itemDto = dataProvider.getItems().stream().filter(actualDto ->
                    actualDto.getId().equalsIgnoreCase(delete.getId().orElse(null))).findFirst().orElse(null);
            dataProvider.getItems().remove(itemDto);
            dataProvider.refreshAll();
        });
        delete.setWidth("10%");
        horizontalLayout.add(delete);
        return horizontalLayout;
    }

    private HorizontalLayout getHeaderLayout() {
        Binder<LabelTableDto> labelTableDtoBinder = new Binder<>();
        labelTableDtoBinder.setBean(new LabelTableDto());

        HorizontalLayout layout = new HorizontalLayout();
        TextField textField = new TextField();
        textField.setClassName("search-field");
        textField.setWidth("90%");
        labelTableDtoBinder.forField(textField).asRequired().bind(LabelTableDto::getValue, LabelTableDto::setValue);
        Button add = VaadinComponents.getButton(VaadinIcon.PLUS.create());
        add.getElement().setProperty(TextValues.TITLE, Transl.get("Add label"));
        layout.add(textField);
        layout.add(add);
        add.addClickListener(buttonClickEvent -> {
            if (labelTableDtoBinder.validate().isOk()) {
                dataProvider.getItems().add(new ItemDto(textField.getValue()));
                dataProvider.refreshAll();
                labelTableDtoBinder.removeBean();
                labelTableDtoBinder.setBean(new LabelTableDto());
            } else {
                ErrorNotification.show(Transl.get("Value in table cannot be empty"), appEnv);
            }
        });
        add.setWidth("10%");
        return layout;
    }

}
