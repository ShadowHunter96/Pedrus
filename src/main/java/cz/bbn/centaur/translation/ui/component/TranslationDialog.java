package cz.bbn.cerberus.translation.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.TranslationComponentOperation;
import cz.bbn.cerberus.translation.dto.TranslationDto;

public class TranslationDialog extends AppDialog {

    private final TranslationComponentOperation componentOperation;

    private final TranslationDto csDto;
    private final TranslationDto enDto;
    private final AppInfiniteGrid<TranslationDto> grid;

    private final AppEnv appEnv;

    public TranslationDialog(TranslationDto csDto, TranslationDto enDto,
                             TranslationComponentOperation componentOperation,
                             AppInfiniteGrid<TranslationDto> grid, AppEnv appEnv) {
        this.csDto = csDto;
        this.enDto = enDto;
        this.componentOperation = componentOperation;
        this.grid = grid;
        this.appEnv = appEnv;
        String title;
        if (csDto.getKey() != null) {
            title = csDto.getKey();
        } else if (enDto.getKey() != null) {
            title = enDto.getKey();
        } else {
            title = Transl.get("New translation");
        }
        setTitle(title);
        initComponent();
    }

    private void initComponent() {

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setSizeFull();

        TranslationDetailComponent csComponent = new TranslationDetailComponent(csDto);
        TranslationDetailComponent enComponent = new TranslationDetailComponent(enDto);

        verticalLayout.add(csComponent, enComponent);
        setContent(verticalLayout);

        addCloseButton();

        Button saveButton = VaadinComponents.getSubmitButton();
        saveButton.setDisableOnClick(true);
        saveButton.addClickListener(e -> {
            componentOperation.saveCsEnDto(csComponent.getDto(), enComponent.getDto());
            this.close();
            SuccessNotification.showSavingSuccess(appEnv);
            grid.loadData();
        });
        addSubmitButton(saveButton);
    }
}
