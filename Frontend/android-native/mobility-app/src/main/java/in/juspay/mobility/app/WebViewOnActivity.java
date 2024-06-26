package in.juspay.mobility.app;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.Bundle;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

public class WebViewOnActivity extends AppCompatActivity {
    protected WebView webView;

    @Override
    public void onBackPressed() {
        if (webView.canGoBack()) {
            webView.post(webView::goBack);
        }
        else super.onBackPressed();
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.web_view_on_activity);
        webView = findViewById(R.id.web);
        Intent intent = getIntent();
        String webViewUrl = intent.getStringExtra("WebViewUrl");
        System.out.println("WebView is :" + webView + " WebView Url " + webViewUrl);
        webView.loadUrl(webViewUrl);
        webView.getSettings().setJavaScriptEnabled(true);
        webView.getSettings().setDomStorageEnabled(true);
        webView.getSettings().setDatabaseEnabled(true);
        webView.setWebViewClient(new WebViewClient());



//        Context context = getApplicationContext();
//
//        super.onCreate(savedInstanceState);
//        bundle = getIntent().getExtras();
//        setContentView(R.layout.reels_player_view);
//        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
//        audioManager  = (AudioManager) getSystemService(Context.AUDIO_SERVICE);
//        if(!isAudioFocusGranted){
//            getAudioFocus();
//        }
//        initializeReelController();
        webView.setWebViewClient(new WebViewClient() {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                System.out.println("Url called is : " + url);
                if (url.startsWith("https://apply.checkrhq-staging.net/static/js/new-success-page")) {
                    goBackToRegistrationScreen();
                    return true;
                }
                return false;
            }
        });
    }

    protected void goBackToRegistrationScreen() {
        super.onBackPressed();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }
}



//public class MainActivity extends AppCompatActivity {
//
//    @Override
//    protected void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);
//        setContentView(R.layout.activity_main);
//
//        // Find the WebView by its unique ID
//        WebView webView = findViewById(R.id.web);
//
//        // loading https://www.geeksforgeeks.org url in the WebView.
//        webView.loadUrl("https://www.geeksforgeeks.org");
//
//        // this will enable the javascript.
//        webView.getSettings().setJavaScriptEnabled(true);
//
//        // WebViewClient allows you to handle
//        // onPageFinished and override Url loading.
//        webView.setWebViewClient(new WebViewClient());
//    }
//}