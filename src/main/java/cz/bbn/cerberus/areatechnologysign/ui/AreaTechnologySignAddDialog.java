package cz.bbn.cerberus.areatechnologysign.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.ValidationResult;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import cz.bbn.cerberus.translation.Transl;

public class AreaTechnologySignAddDialog extends AppDialog {

    private final ObjectType objectType;
    private final String objectId;
    private final ListService listService;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final AppEnv appEnv;
    private final AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent;

    public AreaTechnologySignAddDialog(ObjectType objectType, String objectId, ListService listService,
                                       AreaTechnologyComponentOperation areaTechnologyComponentOperation,
                                       AppEnv appEnv, AreaTechnologySignsBadgeComponent areaTechnologySignsBadgeComponent) {
        this.objectType = objectType;
        this.objectId = objectId;
        this.listService = listService;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.appEnv = appEnv;
        this.areaTechnologySignsBadgeComponent = areaTechnologySignsBadgeComponent;
        initComponent();
    }

    private void initComponent(){
        Binder<AreaTechnologySignDto> binder = new Binder<>();
        AreaTechnologySignDto dto = new AreaTechnologySignDto();
        dto.setObjectType(objectType);
        dto.setObjectId(objectId);

        setTitle(Transl.get("Add sign"));
        HorizontalLayout horizontalLayout = new HorizontalLayout();
        ComboBox<AreaDto> area = new ComboBox<>(Transl.get("Area"));
        ComboBox<TechnologyDto> technology = new ComboBox<>(Transl.get("Technology"));

        area.setItemLabelGenerator(AreaDto::getName);
        area.setItems(listService.getAreaList());
        binder.forField(area).withValidator((areaDto , valueContext)-> {
            if(areaDto == null && technology.getValue() == null){
                return ValidationResult.error(TextValues.CANNOT_BE_EMPTY);
            }
            return ValidationResult.ok();
        }).bind(AreaTechnologySignDto::getAreaDto, AreaTechnologySignDto::setAreaDto);

        technology.setItemLabelGenerator(TechnologyDto::getName);
        technology.setItems(listService.getTechnologyList());
        binder.forField(technology).withValidator((technologyDto , valueContext)-> {
            if(technologyDto == null && area.getValue() == null){
                return ValidationResult.error(TextValues.CANNOT_BE_EMPTY);
            }
            return ValidationResult.ok();
        }).bind(AreaTechnologySignDto::getTechnologyDto, AreaTechnologySignDto::setTechnologyDto);

        horizontalLayout.add(area, technology);
        this.setContent(horizontalLayout);

        binder.setBean(dto);
        addCloseButton();
        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent -> {
            if(binder.validate().isOk()) {
                areaTechnologyComponentOperation.saveAreaTechnologySign(dto, this);
                areaTechnologySignsBadgeComponent.loadData();
            }else{
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
            }
        });
        addSubmitButton(submit);
    }
}
