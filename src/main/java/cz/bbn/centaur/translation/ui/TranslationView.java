package cz.bbn.cerberus.translation.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.TranslationComponentOperation;
import cz.bbn.cerberus.translation.ui.component.TranslationFilterComponent;
import cz.bbn.cerberus.translation.ui.component.TranslationGrid;
import lombok.extern.slf4j.Slf4j;

@Route(value = TranslationView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.TRANSLATION_VIEW)
@Slf4j
public class TranslationView extends AppView {

    public static final String ROUTE = "translation-list";

    private final TranslationComponentOperation componentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public TranslationView(TranslationComponentOperation componentOperation,
                           EntityNewComponentOperation entityNewComponentOperation, AppEnv appEnv) {
        this.componentOperation = componentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
        initView();
    }

    private void initView() {
        setSizeFull();
        Button searchButton = VaadinComponents.getSearchButton();
        AppCardGridComponent appCard =
                new AppCardGridComponent(Transl.get("Translations"), entityNewComponentOperation);
        TranslationFilterComponent filterComponent =
                new TranslationFilterComponent(searchButton, componentOperation.getLangSet());
        appCard.addToContent(filterComponent);
        TranslationGrid translationGrid = new TranslationGrid(componentOperation.getDeleteAction(), appEnv,
                componentOperation.getItemsAction(filterComponent), componentOperation);
        translationGrid.loadData();
        appCard.addToContent(translationGrid);
        searchButton.addClickListener(e -> translationGrid.loadData());
        this.add(appCard);
    }
}
