use serde_json::Value;

use crate::cli::stats::StatsData;
use crate::error::AppError;
use crate::model::{CardDTO, Deck, HardestCardDTO, ImportResult, ReviewSubmission};

/// HTTP client that talks to the turbo-m worker API.
pub struct RemoteClient {
    base_url: String,
    token: String,
    agent: ureq::Agent,
}

impl RemoteClient {
    pub fn new(base_url: String, token: String) -> Self {
        let agent = ureq::Agent::config_builder()
            .http_status_as_error(false)
            .build()
            .into();
        // Strip trailing slash
        let base_url = base_url.trim_end_matches('/').to_string();
        Self {
            base_url,
            token,
            agent,
        }
    }

    fn auth_header(&self) -> String {
        format!("Bearer {}", self.token)
    }

    fn get(&self, path: &str) -> Result<Value, AppError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .agent
            .get(&url)
            .header("Authorization", &self.auth_header())
            .call()
            .map_err(|e| AppError::Http(e.to_string()))?;

        let status = resp.status().as_u16();
        if status >= 400 {
            let body = resp.into_body().read_to_string().unwrap_or_default();
            return Err(AppError::Remote {
                status,
                message: body,
            });
        }

        resp.into_body()
            .read_json()
            .map_err(|e| AppError::Http(e.to_string()))
    }

    fn post_json(&self, path: &str, body: &Value) -> Result<Value, AppError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .agent
            .post(&url)
            .header("Authorization", &self.auth_header())
            .send_json(body)
            .map_err(|e| AppError::Http(e.to_string()))?;

        let status = resp.status().as_u16();
        if status >= 400 {
            let body = resp.into_body().read_to_string().unwrap_or_default();
            return Err(AppError::Remote {
                status,
                message: body,
            });
        }

        resp.into_body()
            .read_json()
            .map_err(|e| AppError::Http(e.to_string()))
    }

    fn put_json(&self, path: &str, body: &Value) -> Result<Value, AppError> {
        let url = format!("{}{}", self.base_url, path);
        let resp = self
            .agent
            .put(&url)
            .header("Authorization", &self.auth_header())
            .send_json(body)
            .map_err(|e| AppError::Http(e.to_string()))?;

        let status = resp.status().as_u16();
        if status >= 400 {
            let body = resp.into_body().read_to_string().unwrap_or_default();
            return Err(AppError::Remote {
                status,
                message: body,
            });
        }

        resp.into_body()
            .read_json()
            .map_err(|e| AppError::Http(e.to_string()))
    }

    pub fn create_deck(&self, name: &str, metadata: Value) -> Result<Deck, AppError> {
        let body = serde_json::json!({ "name": name, "metadata": metadata });
        let resp = self.post_json("/api/decks", &body)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn list_decks(&self) -> Result<Vec<Deck>, AppError> {
        let resp = self.get("/api/decks")?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn update_deck(
        &self,
        name: &str,
        new_name: Option<&str>,
        metadata: Option<Value>,
        fsrs_params: Option<Value>,
    ) -> Result<Deck, AppError> {
        let mut body = serde_json::Map::new();
        if let Some(n) = new_name {
            body.insert("name".to_string(), Value::String(n.to_string()));
        }
        if let Some(m) = metadata {
            body.insert("metadata".to_string(), m);
        }
        if let Some(p) = fsrs_params {
            body.insert("fsrs_params".to_string(), p);
        }
        let path = format!("/api/decks/{}", urlencoding(name));
        let resp = self.put_json(&path, &Value::Object(body))?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn add_card(&self, deck_name: &str, content: Value) -> Result<i64, AppError> {
        let body = serde_json::json!({ "content": content });
        let path = format!("/api/decks/{}/cards", urlencoding(deck_name));
        let resp = self.post_json(&path, &body)?;
        resp.get("id")
            .and_then(|v| v.as_i64())
            .ok_or_else(|| AppError::Http("missing 'id' in response".to_string()))
    }

    pub fn add_cards(&self, deck_name: &str, contents: Vec<Value>) -> Result<Vec<i64>, AppError> {
        let body = serde_json::json!({ "contents": contents });
        let path = format!("/api/decks/{}/cards", urlencoding(deck_name));
        let resp = self.post_json(&path, &body)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn import_cards(
        &self,
        deck_name: &str,
        contents: Vec<Value>,
    ) -> Result<ImportResult, AppError> {
        let body = serde_json::json!({ "contents": contents });
        let path = format!("/api/decks/{}/import", urlencoding(deck_name));
        let resp = self.post_json(&path, &body)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn fetch_due(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        let path = format!("/api/decks/{}/due?limit={}", urlencoding(deck_name), limit);
        let resp = self.get(&path)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn count_due(&self, deck_name: &str) -> Result<u32, AppError> {
        let path = format!("/api/decks/{}/count-due", urlencoding(deck_name));
        let resp = self.get(&path)?;
        resp.get("count")
            .and_then(|v| v.as_u64())
            .map(|v| v as u32)
            .ok_or_else(|| AppError::Http("missing 'count' in response".to_string()))
    }

    pub fn fetch_new(&self, deck_name: &str, limit: u32) -> Result<Vec<CardDTO>, AppError> {
        let path = format!("/api/decks/{}/new?limit={}", urlencoding(deck_name), limit);
        let resp = self.get(&path)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn fetch_hardest(
        &self,
        deck_name: &str,
        limit: u32,
    ) -> Result<Vec<HardestCardDTO>, AppError> {
        let path = format!(
            "/api/decks/{}/hardest?limit={}",
            urlencoding(deck_name),
            limit
        );
        let resp = self.get(&path)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }

    pub fn process_reviews(&self, reviews: &[ReviewSubmission]) -> Result<(), AppError> {
        let body = serde_json::to_value(reviews)?;
        self.post_json("/api/reviews", &body)?;
        Ok(())
    }

    pub fn fetch_stats(&self, deck: Option<&str>) -> Result<StatsData, AppError> {
        let path = match deck {
            Some(name) => format!("/api/stats?deck={}", urlencoding(name)),
            None => "/api/stats".to_string(),
        };
        let resp = self.get(&path)?;
        serde_json::from_value(resp).map_err(AppError::Json)
    }
}

/// Simple percent-encoding for URL path segments.
fn urlencoding(s: &str) -> String {
    s.replace('%', "%25")
        .replace(' ', "%20")
        .replace('/', "%2F")
        .replace('?', "%3F")
        .replace('#', "%23")
        .replace('&', "%26")
}
