package cz.bbn.cerberus.activity.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.activity.dto.ActivityByObjectDto;
import cz.bbn.cerberus.activity.dto.ActivityLinkDto;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class ActivityLinkDialog extends AppDialog {

    private final ListService listService;
    private final SaveAction<ActivityLinkDto> saveAction;
    private final AppInfiniteGrid<ActivityByObjectDto> grid;
    private final ActivityLinkDto dto;
    private final AppEnv appEnv;
    private final List<Long> linkedActivityIdList;

    public ActivityLinkDialog(ListService listService, SaveAction<ActivityLinkDto> saveAction,
                              AppInfiniteGrid<ActivityByObjectDto> grid, ActivityLinkDto dto, AppEnv appEnv,
                              List<Long> linkedActivityIdList) {
        this.listService = listService;
        this.saveAction = saveAction;
        this.grid = grid;
        this.dto = dto;
        this.appEnv = appEnv;
        this.linkedActivityIdList = linkedActivityIdList;
        initComponent();
    }

    private void initComponent() {
        Binder<ActivityLinkDto> binder = new Binder<>();
        setTitle(Transl.get("Link activity"));
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMinHeight("7em");
        List<EnumerationDto> enumerationDtoList = listService.getEnumerationDtoList("ACTIVITY", false);
        enumerationDtoList.removeIf(enumerationDto -> linkedActivityIdList.contains(enumerationDto.getId()));
        MultiSelectComboBox<EnumerationDto> enumeration = new MultiSelectComboBox<>(Transl.get("Activity"));
        enumeration.setItems(enumerationDtoList);
        enumeration.setItemLabelGenerator(EnumerationDto::getName);
        binder.forField(enumeration).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ActivityLinkDto::getActivityDtoSet, ActivityLinkDto::setActivityDtoSet);
        enumeration.setWidth("20em");
        verticalLayout.add(enumeration);
        setContent(verticalLayout);

        binder.setBean(dto);

        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent -> {
            if (binder.validate().isOk()) {
                saveAction.saveItem(dto, null);
                grid.loadData();
                this.close();
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
            }
        });

        addCloseButton();
        addSubmitButton(submit);
    }
}
